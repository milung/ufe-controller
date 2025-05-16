import "../services/ufe-registry" // create reference to apply side-effect

export default async () => {
  /// custom elements can be repeatedly registered due to the rebundling in multiple web-components 
  /// (issue with mwc-web components) - this avoids error and only logs the issue
  function safeDecorator(fn) {
    // eslint-disable-next-line func-names
    return function(...args) {
        if( this.get(args[0]))
        {
          console.warn( `Custom element '${args[0]}' is duplicately registered - ignoring the current attempt for registration`);
          return false;
        }
        else return fn.apply(this, args);
    };
  }
  
  customElements.define = safeDecorator(customElements.define);

  // ensure popstate when updating the history to make the navigation and routing consistent with the address bar
  const pushState = window.history.pushState.bind(window.history);
  window.history.pushState = (data: any, title: string, url?: string) => {
    pushState(data, title, url);
    const popStateEvent = new PopStateEvent('popstate', { state: data });
    dispatchEvent(popStateEvent);
    window.dispatchEvent(popStateEvent);
  }
};

const registerServiceWorker = async (swPath: string, scope: string) => {
  if ("serviceWorker" in navigator) {
      try {
          const registration = await navigator.serviceWorker.register(swPath, { scope, type: "module" });
          if (registration.installing) {
              console.log("Service worker installing");
          } else if (registration.waiting) {
              console.log("Service worker installed");
          } else if (registration.active) {
              console.log("Service worker active");
          }
      } catch (error) {
          console.error(`Registration failed with ${error}`);
      }
  }
};

const pwaModeMeta = Array.from(document.getElementsByTagName('meta')).filter( el => el.getAttribute('name') === 'ufe-pwa-mode');
if (pwaModeMeta?.length > 0 && pwaModeMeta[0].getAttribute('content') === 'pwa') {
  console.log("PWA is enabled");
  let swPath = "sw.mjs";
  const swPathMeta = Array.from(document.getElementsByTagName('meta')).filter( el => el.getAttribute('name') === 'ufe-sw-path');
  if (swPathMeta?.length > 0) {
    swPath = swPathMeta[0].getAttribute('content');
  }
  let swScope = "/";
  const swScopeMeta = Array.from(document.getElementsByTagName('meta')).filter( el => el.getAttribute('name') === 'ufe-sw-scope');
  if (swScopeMeta?.length > 0) {
    swScope = swScopeMeta[0].getAttribute('content');
  }
  addEventListener("DOMContentLoaded", () => registerServiceWorker(swPath, swScope));
} else {
  console.log("PWA is disabled");
}

