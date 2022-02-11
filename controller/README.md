# Mikro-Front End Controller and CRD for kubernetes

Implementation of the [Kubernetes Controller] pattern over custom resources specifying front-end web components to be dynamically integrated into a user interface application shell.

THis is experimental concept design of micro-fronts architecture, considering declarative definition of micro-front-ends as part of the kubernetes API custom resource definitions, and leveraging the [web components](https://developer.mozilla.org/en-US/docs/Web/Web_Components) technology. This enables to approach development of particular micro-front ends in a similar way as is done with development of the cloud-native micro services.

## Architecture

The central part of the concept is kubernetes controller - `ufe-controller`, which watches resources of a kind `WebComponent` (see [./deploy/crd.yaml]) deployed to the cluster and compiles the them into the form of the front-end configuration. The embedded web server provides the application shell to integrate the configured web coponents, and proxies the requests to mikro front end server.

The microfrontends are assumed to serve web-component package module, with dedicated web component to serve as a micro front. It can also serve various other elements to be display in specific contexts.

The project is of educational nature.

## Installation

The controller is provided as a configurable docker image [milung/ufe-controller](https://hub.docker.com/repository/docker/milung/ufe-controller). It can be deployed into the kubernetes cluster using the manifests in the [./deploy] folder, which also includes `kustomization.yaml` manifest. In the default setup the controller starts to observe WebComponent resources in all namespaces.

After installation you can navigate to `ufe-controller` web ui, e.g. executing the command

```bash
kubectl port-forward service/ufe-controller 8080:80
```

and navigating the browser to [http://localhost:8080]. You should see an empty application shall waiting for some `WebComponent`-s being deployed to the cluster.

A sample deployment with a demo web application is available in the folder [./example]

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

## Configuration

TBD. 