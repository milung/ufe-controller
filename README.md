# Micro-Front End Controller and CRD for kubernetes

Implementation of the [Kubernetes Controller] pattern over custom resources specifying front-end web components to be dynamically integrated into a user interface application shell.

This is experimental concept design of micro-fronts architecture, considering declarative definition of micro-front-ends as part of the kubernetes API custom resource definitions, and leveraging the [web components](https://developer.mozilla.org/en-US/docs/Web/Web_Components) technology. This enables to approach development of particular micro-front ends in a similar way as is done with development of the cloud-native micro services.

## Architecture

The central part of the concept is kubernetes controller - `ufe-controller`, which watches resources of a kind `WebComponent` (see [./deploy/crd.yaml]) deployed to the cluster and compiles the them into the form of the front-end configuration. The embedded web server provides the application shell to integrate the configured web coponents, and proxies the requests to mikro front end server.

The microfrontends are assumed to serve web-component package module, with dedicated web component to serve as a micro front. It can also serve various other elements to be display in specific contexts.

The project is of educational nature.

## Installation

The controller is provided as a configurable docker image [milung/ufe-controller](https://hub.docker.com/repository/docker/milung/ufe-controller). It can be deployed into the kubernetes cluster using the manifests in the [./configs/k8s/kustomize] folder, which also includes `kustomization.yaml` manifest. In the default setup the controller starts to observe WebComponent resources in all namespaces.

After installation you can navigate to `ufe-controller` web ui, e.g. executing the command

```bash
kubectl port-forward service/ufe-controller 8080:80
```

and navigating the browser to [http://localhost:8080]. You should see an empty application shall waiting for some `WebComponent`-s being deployed to the cluster.

A sample deployment with a demo web application is available in the folder [./examples/kustomize]

## Usage

Let's assume your micro front-end is implementing a custom web component with element tag `my-web-app`, and is served by a kubernetes service `my-frontend` in the namespace  `my-namespace`. To integrate this web component application into the controller`s application shell, the following resource has to be deployed to the cluster:

```yaml
apiVersion: fe.milung.eu/v1
kind: WebComponent
metadata: 
  name: my-web-app
spec:   
  module-uri: http://my-frontend.my-namespace/modules/web-components.esm.js  
                     # it is recommended to always use namespaced domain of the service, as ufe controller 
                     # can be running in different namespace 
  navigation:
    - element: my-web-app    # element tag to  use in app shell when navigating to /my-web-page,
      path: my-web-page      # when user navigates to subpath ./my-web-page, the specific element 
                             # will gain controll of the application shell`s content area
      title: My Wanderfull Micro App # title to be displayed to end user (e.g. on landing page)
      details: This is my wanderfull example functionality
  preload: false             # if set to true the module will be loaded imediately after loading 
                             # the landing page, otherwise it will be loaded only when needed
  proxy: true                # if set to false then module uri must be accessible from the user network
                             # and may require futher configuration to enable cross origin loading
  hash-suffix: v1alpha1      # optional suffix when proxy-ing the module. Changing it value will force 
                             # to refresh cache, and avoids issues with cached versus actual version
```

The full specification of custom resource can be seen in [crd.yaml](./configs/k8s/kustomize/crd.yaml)

## Server Configuration

The backend of the controller can be configured by setting environment variables, below is a list of the currently supported variable:

| Env. Variable | Default Value | Description |
|- |- |- |
|ACCEPTS_LANGUAGES|en|List of semicolon, or comma separated language codes that are supported. If there is match between `Accept-Language` header and this list, then language of html element is set to such language. In case there is no match then html language is set to the first language in this list|
|APP_ICON_LARGE|./assets/icon/icon.png|link to application icon used in manifest" Shall be 512*512 pixels|
|APP_ICON_SMALL|./assets/icon/icon.png|link to application icon used in manifest" Shall be 64*64 pixels|
|APPLICATION_DESCRIPTION||Some detailed description of the applivation to be part of the `index.html` meta. Language specific descriptions are also possible, e.g. APPLICATION_DESCRIPTION_EN_US|
|APPLICATION_SHELL_CONTEXT|application-shell|context of the dynamic web component that is used to retrieve the application shell - used to build the top-level element in the page body|
|APPLICATION_TITLE_SHORT|Shell|Short version of the language fallback application title, language specific titles are also possible, e.g. APPLICATION_TITLE_SHORT_EN_US|
|APPLICATION_TITLE|Application shell|Language fallback application title, language specific titles are also possible, e.g. APPLICATION_TITLE_EN_US|
|BASE_URL|\\ |Base URL of the server, all absolute links are prefixed with this address|
|FAVICON_ICO|./assets/icon/favicon.ico|link to favicon used as if in`<link rel="icon" href="${FAVICON}">`|
|FORCED_REFRESH_PERIOD_SECONDS|60|Period in seconds, when the configuration is forced to be refreshed independently of the k8s watching status|
|HTTP_CSP_HEADER|default-src 'self'; font-src 'self'; script-src 'strict-dynamic' 'nonce-{NONCE_VALUE}'; worker-src 'self'; manifest-src 'self'; style-src 'self' 'strict-dynamic';|Content Security Policy header directives for serving the root SPA html page. The placeholder `{NONCE_VALUE}` will be automatically replaced by the random nonce text used to augment `<script>` elements in the html file.|
|HTTP_PORT|80|HTTP port the server is listening on.|
|MANIFEST_TEMPLATE|manifest.template.json| Path to the manifest.json template file to be used when registering PWA application. The path must be within the scope of the `/app/www` folder and relative to it. The file may contains mustache plaholders.|
|OBSERVE_NAMESPACES||Comma separated list of namespaces in which to look for webcomponents to be served by this instance|
|PWA_MODE|disabled|_(experimental)_ if set to "pwa" then service worker will be registered and PWA functionality will be provided by the service worker|
|SERVICE_WORKER|sw.mjs|Path to the service worker script. Absolute path will work on any SPA subpage|
|SERVICE_WORKER_SCOPE|/|Scope of service worker |
|SW_VERSION|v1|Version of the service worker, used to force the browser to update the service worker|
|SW_SKIP_FETCH| |Comma separated list of regular expressions against request paths which should not be fetched by the service worker. All paths that contains `/api/` string, or requests to other domains are implicitly skipped reagrdless of this setting. All other requests, including requests toward web components are served with cache-first startegy|
|TOUCH_ICON|./assets/icon/icon.png|link to favicon used as if in `<link rel="apple-touch-icon" hred="${TOUCH_ICON}"`|
|USER_ID_HEADER|x-forwarded-email|incomming request`s header name (lowercase) specifying the user identifier, typically email|
|USER_NAME_HEADER|x-forwarded-user|incomming request`s header name (lowercase) specifying the user name|
|USER_ROLES_HEADER|x-forwarded-groups|incomming request`s header name (lowercase) specifying the list of user roles (or groups)|
|WEBCOMPONENTS_SELECTOR||comma separate list of key-value pairs, used to filter WebComponent resources handled by this controller|


The project is of educational nature

## Server endpoints

_All endpoints may be prefixed by `BASE_URL` path._

| Endpoint  | Description |
|- |- |
| `/app-icons/<navigation path>`| The `navigation` entry may specify the icon for the application, to be used in the fronted. In such case the icon can be retrieved under this endpoint, given the `<navigation path>` matches the property `path` of the given navigation entry. |
| `/fe_config` | Serves `application/json` object that describes the current applications, context and modules collected by the controller. Used in the frontend for dynamic loading of the web components. See interface `UfeCOnfiguration` in [./web-ui/src/services/ufe-registry.tsx] for the type definition. |
| `/healtz`| Health check of the controller |
| `/web-components/<namespace>/<name>` | In the case the `WebComponent` witn the matadata `<name>` and `<namespace>` is configured with the property `proxy: true`, then its module and all relative assets are served under this path |
| `/` | All other paths are routed to frontend single page applicatio - see below description. |

## Application Shell Configuration

The `index.html` page is initially empty and loads the `/fe_config` json object, that describes the applications, contexts, and a basic user identity. The object is exposed at `window.ufeRegistry`, if you need a direct access. Once the page loads, the script decides, which web component to load as an application shell. It will use the built-in web component with the element tag `ufe-default-shell`. It is possible to to replace the application shell by configuring the  controller with the environment variable `APPLICATION_SHELL_CONTEXT` and registering `WebComponent` with  such context element. Eventually, the complete built- in user interface may be ignored, and some custom front-end application shell used, while using only `ufe-controller` endpoints described above.

The static resources for the UI are under the path `/app/www`, you may eventually mount additional assets there, or replace the prepared assets. When serving the [`index.html`](./web-ui/src/index.html), the controller preprocess it and replaces some parts with predefined environment variables, using the [{{mustache}}](https://mustache.github.io/) syntax. Additionally, all script elements in the `index.html` has added dynamically generated [nonce](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/nonce).

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
