import { Component, Prop, h } from '@stencil/core';

@Component({
  tag: 'ufe-account-app',
  styleUrl: 'ufe-account-app.css',
  shadow: true,
})
export class uFeAccountApp {
  
  @Prop() identity: string;

  @Prop() userName: string;

  

  render() {
    
    return (
    <div class="ufe-app" >
        <div >
          <ufe-avatar email={this.identity}></ufe-avatar>
        </div>
        <div>
          <h2>{this.userName}</h2>
          <i>{this.identity}</i >
      </div>
    </div>
    );
  }
}
