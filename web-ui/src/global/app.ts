export default async () => {
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
};
