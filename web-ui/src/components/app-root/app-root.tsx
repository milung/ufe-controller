import { Component, forceUpdate, h, Element } from '@stencil/core';
import '@material/mwc-top-app-bar-fixed';
import '@material/mwc-icon-button';
import '@material/mwc-drawer';
import { createRouter, Route, href } from 'stencil-router-v2';



const Router = createRouter();

@Component({
  tag: 'app-root',
  styleUrl: 'app-root.css',
  shadow: true,
})
export class AppRoot {

  private static webConfig = null;
  
  @Element() element: HTMLElement;

  private async loadComponents() { 
    if(AppRoot.webConfig != null) {
      return AppRoot;
    }
    else {
      let response = await fetch('/fe-config');
      AppRoot.webConfig  = await response.json();
    }
  }

  async componentWillLoad() {
    return await this.loadComponents();
  }

  app_render(app) {
    let content = `<${app.element}`;
    app.attributes.forEach(attribute => {
      content += ` ${attribute.name}="${attribute.value}"`;
    });
    content += `></${app.element}>`;
    return (
      <Route 
        path={'/' + app.path}
        render={ () => {
          let url = app.load_url;
          import(url);          
          return <div class="application-area" innerHTML={content}></div>;
        }} />
    )
  }

  componentDidLoad() {
    const drawer = this.element.shadowRoot.querySelectorAll('mwc-drawer')[0];
    if (drawer) {
        const container = drawer.parentNode;
        container.addEventListener('MDCTopAppBar:nav', () => {
            drawer.open = !drawer.open;
        });
    }
  }

  render() {
    let title = 'App Shell';
    let apps = AppRoot.webConfig != null ? AppRoot.webConfig.apps : [];
    return (
        <mwc-drawer hasHeader type="dismissible" open>
          <span slot="title">{title}</span>
          {/* <span slot="subtitle">subtitle</span> */}
          <div>
              <ul>
              { apps.map( app => 
                <li>
                  <div class='app-title'>
                    <a {...href(app.path)}>{app.title}</a>
                  </div> 
                  { apps.details
                    ? <div class='app-details'>{app.details}</div>
                    : <span/>
                  }
                </li>
                  
              )}
              </ul>
          </div>
          <div slot="appContent">
            <mwc-top-app-bar-fixed>
              <mwc-icon-button icon="menu" slot="navigationIcon" ></mwc-icon-button>
              <div slot="title">{title}</div>
              <mwc-icon-button icon="file_download" slot="actionItems"></mwc-icon-button>
              <mwc-icon-button icon="print" slot="actionItems"></mwc-icon-button>
              <mwc-icon-button icon="favorite" slot="actionItems"></mwc-icon-button>
              <main>

              <Router.Switch>
                {apps.map(this.app_render)}
                <Route 
                  path="/"
                  render={ () => {
                    return (<p>Welcome to the new stencil-router demo</p>);
                  }}
                />
              

              </Router.Switch>
      
              </main>
            </mwc-top-app-bar-fixed>
          </div>
        </mwc-drawer>
    );
  }
}

