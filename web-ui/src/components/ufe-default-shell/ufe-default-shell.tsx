import { Component, h, Element, State } from '@stencil/core';
import '@material/mwc-top-app-bar-fixed';
import '@material/mwc-icon-button';
import '@material/mwc-drawer';
import { href, Route } from 'stencil-router-v2';
import { getUfeRegistryAsync, UfeRegistry } from '../../services/ufe-registry';



@Component({
  tag: 'ufe-default-shell',
  styleUrl: 'ufe-default-shell.css',
  shadow: true
})
export class UfeDefaultShell {

  private ufeRegistry: UfeRegistry;
  
  @Element() element: HTMLElement;

  @State() appTitle: string = document.title;

  async componentWillLoad() {
    this.ufeRegistry = await getUfeRegistryAsync();
  }

  app_render(app) {
    const appPath = this.ufeRegistry.basePath + app.path;
    let content = `<${app.element}`;
    const attributes = [ ...app.attributes,  {name: "base-path", value: appPath } ];
    attributes.forEach(attribute => {
      content += ` ${attribute.name}="${attribute.value}"`;
    });
    content += `></${app.element}>`;
    
    return (
      <Route 
        path={new RegExp('(^' + appPath + '\/|^' + appPath + '$)')}
        render={ () => {
          if (this.appTitle != app.title) {
            setTimeout(() => {this.appTitle = app.title}, 0);
          };
          let url = app.load_url;
          if(url?.length) { import(url); }        
          return <div class="application-area" innerHTML={content}></div>;
        }} />
    )
  }

  componentDidLoad() {
    const drawer = this.element.shadowRoot.querySelectorAll('mwc-drawer')[0] as any;
    if (drawer) {
        const container = drawer.parentNode;
        container.addEventListener('MDCTopAppBar:nav', () => {
            drawer.open = !drawer.open;
        });
    }
  }

  render() {
    const title = this.appTitle;
    document.title = title;
    const apps = this.ufeRegistry.navigableApps();
    const Router = this.ufeRegistry.router;
    const vw = Math.max(document.documentElement.clientWidth || 0, window.innerWidth || 0)
    const base = this.ufeRegistry.basePath;

    return (        
        <mwc-drawer hasHeader type={vw < 800 ? "modal" : "dismissible"} open={vw > 640}>
          <span slot="title">{title}</span>
          {/* <span slot="subtitle">subtitle</span> */}
          <div>
              <ufe-applications-list router={Router}></ufe-applications-list>
          </div>
          <div slot="appContent">
            <mwc-top-app-bar-fixed>
              <mwc-icon-button icon="menu" slot="navigationIcon" ></mwc-icon-button>
              <div slot="title">{title}</div>
              <mwc-icon-button icon="home" slot="actionItems" {...href(base)}></mwc-icon-button>
              <ufe-context context="app-bar-tools" slot="actionItems"></ufe-context>
              <main>
                <Router.Switch>
                  {apps.map(app => this.app_render(app))}
                  <Route path={ p => p == "/" || p == base || p == base.substring(0, base.length-1)} >
                    <ufe-application-cards></ufe-application-cards>
                  </Route>
                  <Route path={() => true}>
                    <error-page status-code="404" path={document.location.pathname}>
                        <div class="error-no-modules">
                            <h1> 404: Not Found </h1> 
                            <div class="path">{document.location.pathname}</div>
                        </div>
                        <a slot="home" {...href(base)}>Navigate to the landing page</a>
                    </error-page>
                  </Route>
                </Router.Switch>    
              </main>
            </mwc-top-app-bar-fixed>
          </div>
        </mwc-drawer>
    );
  }
}

