import { Component, Host, h, Prop } from '@stencil/core';
import { Router } from 'stencil-router-v2';
import { getUfeRegistryAsync, UfeRegistry } from '../../services/ufe-registry';
import '@material/mwc-button';

@Component({
  tag: 'ufe-application-cards',
  styleUrl: 'ufe-application-cards.css',
  shadow: true,
})
export class UfeApplicationCards {

  @Prop()
  selector: string | {[label:string]: string};

  @Prop()
  router: Router;

  private ufeRegistry: UfeRegistry;

  public async componentWillLoad() {
    this.ufeRegistry = await getUfeRegistryAsync();
  }

  private getSelectorObject(): {[label:string]: string} {
     return {};
  }
  public render() {
    const apps = this.ufeRegistry.navigableApps( this.getSelectorObject() )
    return (
      <Host>
        {apps.map(app => 
        <div class="card"><div class="card-wrapper">
          <div class="title-section">
            { app.icon?.length
            ? <img src={app.icon} class="card-icon"></img>
            : <noop></noop>
            }
            <div class="title">{app.title}</div>
          </div>
          <div class="text">{app.details}</div>
          <div class="fill-1"></div>
          <div class="actions">
            <mwc-button label="Otvoriť" {...this.ufeRegistry.href(app.path, this.router)}/>
          </div></div>
        </div> 
        )}
      </Host>
    );
  }

}
