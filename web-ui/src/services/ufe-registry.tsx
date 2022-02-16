import { createRouter, Router } from "stencil-router-v2";

export interface UfeElement {
    element: string,
    attributes: Array<{name: string, value: any}>,
    labels?: {[name: string]: string},
    load_url: string,
    roles?: string[]
}

export interface UfeWebApp extends UfeElement{
    title: string;
    details: string;
    path: string;
    priority: number;
    icon?: string
}

export interface UfeContext extends UfeElement {
    contextNames: string[];
}


interface UfeConfiguration {
    preload: string[];
    apps: UfeWebApp[];
    contexts: UfeContext[];
}

export interface UfeRegistry {

    router: Router;

    href(href, router?: Router): {href: any; onClick: (ev: any) => void;};

    navigableApps(selector?: { [name: string]: string} ): UfeWebApp[] ;

    contextElements(context: string, selector?: { [name: string]: string}): UfeContext[] ;

    preloadDependenciesAsync(elements: UfeElement[]): Promise<void>;

    elementHtmlText(element: UfeElement): string;
}

class UfeRegistryImpl implements UfeRegistry{

    private constructor() {};

    private static webConfig: UfeConfiguration = null;

    public readonly router: Router = createRouter();

    public href(href, router = this.router)  {
        return {
            href,
            onClick: (ev) => {
                if (ev?.metaKey || ev?.ctrlKey) {
                    return;
                }
                if (ev?.which == 2 || ev?.button == 1) {
                    return;
                }
                ev?.preventDefault();
                router?.push(href);
            },
        };
    };

    static async loadComponents() {
        if(UfeRegistryImpl.webConfig != null) {
            return UfeRegistryImpl.webConfig;
        }
        else {
            let response = await fetch('/fe-config');
            UfeRegistryImpl.webConfig  = await response.json();
            let preloads = UfeRegistryImpl.webConfig != null ? UfeRegistryImpl.webConfig.preload : [];
            preloads.forEach( url => { if(url?.length) import(url);})
        }
    }

    static async instanceAsync(nowait:boolean = false): Promise<UfeRegistry> {
        return new Promise(async ( resolve, _) => {
            while(!nowait && !((window as any).ufeRegistry) )
            {
                await new Promise(resolve => setTimeout(resolve, 250));
            }
            resolve(new UfeRegistryImpl());
        })
    }

    navigableApps(selector: { [name: string]: string} = {} ): UfeWebApp[]  {
        return this
            .matchSelector(selector, UfeRegistryImpl.webConfig.apps)
            .sort( (a, b) => b.priority  - a.priority);
    }

    contextElements(context: string, selector: { [name: string]: string} = {}): UfeContext[]  {
        return this
            .matchSelector(selector, UfeRegistryImpl.webConfig.contexts)
            .filter(_ => _.contextNames.includes(context));
    }

    elementHtmlText(app: UfeElement): string {
        let content = `<${app.element}`;
        app.attributes.forEach(attribute => {
        content += ` ${attribute.name}="${attribute.value}"`;
        });
        content += `></${app.element}>`;
        return content;
    }

    async preloadDependenciesAsync(elements: UfeElement[]) {
        const loads = [...new Set(elements
            .filter(_ => _.load_url?.length)
            .map(_ => _.load_url))]
            .map(_ => import(_) as Promise<{}>);
        await Promise.all(loads);
    }

    private matchSelector<T extends UfeElement>(selector: string | { [name: string]: string} | undefined, elements: T[]): T[]  {
        if( elements === undefined  || elements === null || elements === []) return [];
        const metas =  document.getElementsByTagName('meta');
        let normalizedSelector: { [name: string]: string};
        // normalize selector
        if(selector === undefined) {
            normalizedSelector = {};
        } else if (typeof selector === 'string') {
            normalizedSelector = this.splitSelectorString(selector);
        } else {
            normalizedSelector = {...selector};
        }
        // combine with page selector specifier
        for (let i = 0; i < metas.length; i++) {
            if (metas[i].getAttribute('name') === "ufe-selector") {
              const content =  metas[i].getAttribute('content');
              if(content) {
                  selector = { ...normalizedSelector, ...this.splitSelectorString(content)};
              }
            }
        }
        // filter applications by selector
        return elements.filter(element => 
            Object.keys(selector)
                  .every( labelName => 
                    selector[labelName] === element.labels[labelName])); 
    };

    private splitSelectorString( selector: string) : { [label: string]: string } {
        return selector
                .split(/(\,|;)/)
                .map(_=> _.split('=', 2))
                .reduce( (acc, keyValue) => { acc[keyValue[0]] = keyValue[1]; return acc }, {} );
    }

    async createAppShell() {
        const registry = await UfeRegistryImpl.instanceAsync();
        const metas =  document.getElementsByTagName('meta');
        var context = "";
        for (let i = 0; i < metas.length; i++) {
            if (metas[i].getAttribute('name') === "ufe-shell-context") {
              context =  metas[i].getAttribute('content');
              break;
            }
          }
        const shell = registry.contextElements(context, {})[0] || {
            element: "ufe-default-shell",
            attributes: [],
            load_url: "",
            roles: ["*"]
        };
        await registry.preloadDependenciesAsync([shell]);

        const element = document.createElement(shell.element);
        Object.keys(shell.attributes).forEach( attribute => element[attribute] = shell.attributes[attribute]);
        
        document.body.appendChild(element);
    }
}

export const getUfeRegistryAsync = () => UfeRegistryImpl.instanceAsync(false);


window.addEventListener("load", async _ => {
    if((window as any).ufeRegistry) return;

    await UfeRegistryImpl.loadComponents();
    (window as any).ufeRegistry = await UfeRegistryImpl.instanceAsync(true)
    await (window as any).ufeRegistry.createAppShell();
    
})