import { Component, h, Element, State } from '@stencil/core';
import '@material/mwc-top-app-bar-fixed';
import '@material/mwc-icon-button';
import '@material/mwc-drawer';
import { href } from 'stencil-router-v2';
import { getUfeRegistryAsync, UfeRegistry } from 'ufe-registry';



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
    const Router = this.ufeRegistry.router;
    const vw = Math.max(document.documentElement.clientWidth || 0, window.innerWidth || 0)
    const base = this.ufeRegistry.basePath;

    return (        
        <mwc-drawer hasHeader type={vw < 800 ? "modal" : "dismissible"} >
          <span slot="title">{title}</span>
          {/* <span slot="subtitle">subtitle</span> */}
          <div>
              <ufe-applications-list router={Router}></ufe-applications-list>
          </div>
          <div slot="appContent">
            <mwc-top-app-bar-fixed>
              <mwc-icon-button icon="menu" slot="navigationIcon" ></mwc-icon-button>
              <div slot="title" class="app-title">{title}</div>
              <mwc-icon-button icon="home" slot="actionItems" {...href(base)}></mwc-icon-button>
              <ufe-context context="app-bar-tools" slot="actionItems"></ufe-context>
              <main>
                <ufe-app-router></ufe-app-router>
              </main>
            </mwc-top-app-bar-fixed>
          </div>
        </mwc-drawer>
    );
  }
}

