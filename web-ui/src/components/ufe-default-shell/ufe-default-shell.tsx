import { Component, h, Element } from '@stencil/core';
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

  async componentWillLoad() {
    this.ufeRegistry = await getUfeRegistryAsync();
  }

  app_render(app) {
    let content = `<${app.element}`;
    app.attributes.forEach(attribute => {
      content += ` ${attribute.name}="${attribute.value}"`;
    });
    content += `></${app.element}>`;
    return (
      <Route 
        path={new RegExp('(^\/' + app.path + '\/|^\/' + app.path + '$)')}
        render={ () => {
          let url = app.load_url;
          import(url);          
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
    const title = document.title;
    const apps = this.ufeRegistry.navigableApps();
    const Router = this.ufeRegistry.router;

    return (        
        <mwc-drawer hasHeader type="dismissible" open>
          <span slot="title">{title}</span>
          {/* <span slot="subtitle">subtitle</span> */}
          <div>
              <ufe-applications-list router={Router}></ufe-applications-list>
          </div>
          <div slot="appContent">
            <mwc-top-app-bar-fixed>
              <mwc-icon-button icon="menu" slot="navigationIcon" ></mwc-icon-button>
              <div slot="title">{title}</div>
              <mwc-icon-button icon="home" slot="actionItems" {...href("/")}></mwc-icon-button>
              <main>
                <Router.Switch>
                  {apps.map(this.app_render)}
                  <Route path="/">
                    <ufe-application-cards></ufe-application-cards>
                  </Route>
                </Router.Switch>      
              </main>
            </mwc-top-app-bar-fixed>
          </div>
        </mwc-drawer>
    );
  }
}

