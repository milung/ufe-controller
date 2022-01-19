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
