import { Component, Host, h, Prop } from '@stencil/core';
import { getUfeRegistryAsync, UfeRegistry } from 'ufe-registry';

@Component({
  tag: 'ufe-context',
  styleUrl: 'ufe-context.css',
  shadow: true,
})
export class UfeContext {

  // name of the context to render web components from contex configurations
  @Prop() context: string 

  // selector to match the elements to render
  @Prop() selector: string ;

  // attributes will be propagated to rendered web components
  @Prop() attributes: { [key: string]: string}

  // any data, 
  @Prop() data: {
    [key: string]: any
  };

  private ufeRegistry: UfeRegistry;

  public async componentWillLoad() {
    this.ufeRegistry = await getUfeRegistryAsync();
  }

 
  render() {
    const contextElements = this.ufeRegistry.contextElements( this.context, this.selector );
    this.ufeRegistry.preloadDependenciesAsync(contextElements);
    
    return (
      
      <Host>
        <slot name="beforeALl"></slot>
        {contextElements.map( el => { 
          const element = this.ufeRegistry.loadAndRenderElement(el, Object.assign({}, this.attributes, this.data));
          return (
            <context-item>
              <slot name="before-each"></slot>
              { element }
              <slot name="after-each"></slot>
            </context-item>
          )
        })}
        <slot name="afterAll"></slot>
      </Host>
    );
  }

}
