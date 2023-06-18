## Disclaimer
This repo was forked from, is using and is heavily inspired by https://github.com/milung/ufe-controller.
The goal of this project is to rewrite the UI server part of the ufe-controller to go and separate it from the Backend part.
WebUI is mainly reused and go server is used to serve the UI.

## Micro Frontend Example WebUI Application Shell Configuration

The `index.html` page is initially empty and loads the `/fe_config` json object, that describes the applications, contexts, and a basic user identity. The object is exposed at `window.ufeRegistry`, if you need a direct access. Once the page loads, the script decides, which web component to load as an application shell. It will use the built-in web component with the element tag `ufe-default-shell`. It is possible to replace the application shell by configuring the  controller with the environment variable `APPLICATION_SHELL_CONTEXT` and registering `WebComponent` with  such context element. This serves as an example web ui for microfrontend controller and can be replaced with your own UI at any time.

The static resources for the UI are under the path `/app/www`, you may eventually mount additional assets there, or replace the prepared assets. When serving the [`index.html`](./web-ui/src/index.html), the controller preprocess it and replaces some parts with predefined environment variables, using the [{{.Template}}] go template syntax. Additionally, all script elements in the `index.html` has added dynamically generated [nonce](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/nonce).

In the case you want to load content from  other origins, you may need to adapt the environment variable `HTTP_CSP_HEADER`, otherwise the request will be blocked by browsers.

## Built-in web components

Following web components are available for use in the hosted web components:

* `ufe-app-router` application router to host the current path's application as  specified by the navifation section in CRD. The attribute `home-component` specifies which component shall be hosted at the root path - defaults to `ufe-application-card`

* `ufe-application-cards` - displays a card per registered navigation section in CRD. Attribute `selector` allows to narrow the list of applications/navigations based on their labels.

* `ufe-application-cards` - similar to above but displays a `mwc-list` of application titles.

* `ufe-context` - display sequence of the elements mentioned in the CRD's resources under `context elements` sections, that matches attribute `context`. Attribute `selector` allows to futher narrow the list of the elements by the elements labels.

  This element accepts following slots:
  * `beforeAll` - placed before the sequence of the elements being displayed
  * `afterAll` - placed after the sequence of the elements being displayed
  * `beforeEach` - placed before each element being displayed
  * `afterEach` - placed after each element being displayed

## Examples for customized shell

See also [ufe-registry](https://www.npmjs.com/package/ufe-registry) package

* Creating custom list of navigable elements and placeholder for displaying the current app:

  ```ts
  import { Component, Host, h, State, Prop } from '@stencil/core';
  import { Router } from 'stencil-router-v2';
  import { getUfeRegistryAsync, UfeRegistry} from "ufe-registry"

  @Component({
    tag: 'my-shell',
    styleUrl: 'my-shell.css',
    shadow: true,
  })
  export class MyShell {

    @Prop() router: Router; // use subrouter if your app is hosted in another web-component
    
    ufeRegistry: UfeRegistry;

    async componentWillLoad() {
      this.ufeRegistry = await getUfeRegistryAsync() // wait for UfeRegistry being available
    }
    
    render() {
      const apps = this.ufeRegistry.navigableApps() // get list of application registered in cluster
      <my-shell>
        <navigation-panel>
            <tabs>
              {apps.map( app => {
                const active = false
                (<app-tab
                    label={app.title} 
                    {...this.ufeRegistry.href(app.path, this.router || this.ufeRegistry.router)}
                    active={app.isActive} ></app-tab>
                )})}
            </tabs>    
        </navigation-panel>
        <ufe-app-router></ufe-app-router>   // shows the webcomponent of the currently active app
      </my-shell>
    }
  ```
