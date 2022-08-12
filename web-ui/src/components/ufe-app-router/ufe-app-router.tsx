import { Component, Host, h, State, Prop } from '@stencil/core';
import { Route, href } from 'stencil-router-v2';
import { getUfeRegistryAsync, UfeRegistry } from '../../services/ufe-registry';

@Component({
  tag: 'ufe-app-router',
  styleUrl: 'ufe-app-router.css',
  shadow: true,
})
export class UfeAppRouter {

  private ufeRegistry: UfeRegistry;
  
  @State() appTitle: string = document.title;
  @Prop() homeComponent: string = "ufe-application-cards";
  
  async componentWillLoad() {
    this.ufeRegistry = await getUfeRegistryAsync();
  }

  app_render(app) {
    const appPath = this.ufeRegistry.basePath + app.path;
    let content = `<${app.element}`;
    const attributes = [  {name: "base-path", value: appPath }, ...app.attributes  ];
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

  render() {
   
    const apps = this.ufeRegistry.navigableApps();
    const Router = this.ufeRegistry.router;
    const base = this.ufeRegistry.basePath;
    const Home = this.homeComponent;

    return (
      <Host>
        <Router.Switch>
            {apps.map(app => this.app_render(app))}
            <Route path={ p => p == "/" || p == base || p == base.substring(0, base.length-1)} >
              <Home></Home>
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
      </Host>
    );
  }

}
