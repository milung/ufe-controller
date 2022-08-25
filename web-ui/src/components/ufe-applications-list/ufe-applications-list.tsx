import { Component, Host, h, Prop } from '@stencil/core';
import { getUfeRegistryAsync, UfeRegistry } from 'ufe-registry';
import { Router } from 'stencil-router-v2';
import '@material/mwc-list'; 
import '@material/mwc-icon';

@Component({
  tag: 'ufe-applications-list',
  styleUrl: 'ufe-applications-list.css',
  shadow: true,
})
export class UfeApplicationsList {

  @Prop()
  selector: string ;

  @Prop()
  router: Router;

  private ufeRegistry: UfeRegistry;

  public async componentWillLoad() {
    this.ufeRegistry = await getUfeRegistryAsync();
  }

  public render() {
    const apps = this.ufeRegistry.navigableApps( this.selector )
    return (
      <Host>
        <mwc-list>
          {apps.map( app => (
            <mwc-list-item {...this.ufeRegistry.href(app.path, this.router)}>{app.title}</mwc-list-item>
          ))}
        </mwc-list>
      </Host>
    );
  }

}
