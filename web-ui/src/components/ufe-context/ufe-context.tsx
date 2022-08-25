import { Component, Host, h, Prop } from '@stencil/core';
import { getUfeRegistryAsync, UfeRegistry } from 'ufe-registry';

@Component({
  tag: 'ufe-context',
  styleUrl: 'ufe-context.css',
  shadow: true,
})
export class UfeContext {

  @Prop() context: string 

  @Prop() selector: string ;

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
