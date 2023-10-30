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

  // if set, then the "data" property will be assigned to the rendered components
  @Prop() data: any;

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
          el.attributes = Object.assign({}, el.attributes, this.attributes);
          return (
            <context-item>
              <slot name="before-each"></slot>
              <span innerHTML={this.ufeRegistry.elementHtmlText(el)}></span>
              <slot name="after-each"></slot>
            </context-item>
          )
        })}
        <slot name="afterAll"></slot>
      </Host>
    );
  }

}
