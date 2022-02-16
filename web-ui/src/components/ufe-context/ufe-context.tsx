import { Component, Host, h, Prop } from '@stencil/core';
import { getUfeRegistryAsync, UfeRegistry } from '../../services/ufe-registry';

@Component({
  tag: 'ufe-context',
  styleUrl: 'ufe-context.css',
  shadow: true,
})
export class UfeContext {

  @Prop() context: string 

  @Prop() selector: string | {[label:string]: string};

  private ufeRegistry: UfeRegistry;

  public async componentWillLoad() {
    this.ufeRegistry = await getUfeRegistryAsync();
  }

  private getSelectorObject(): {[label:string]: string} {
     return {};
  }

  
  render() {
    const contextElements = this.ufeRegistry.contextElements( this.context, this.getSelectorObject() );
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
