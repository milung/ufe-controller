import { Component, Host, h, Prop } from '@stencil/core';

@Component({
  tag: 'ufe-frame',
  styleUrl: 'ufe-frame.css',
  shadow: true,
})
export class UfeFrame {

  @Prop() src: string;
  @Prop() width: string;
  @Prop() height: string;

  render() {
    return (
      <Host>
        <iframe src={this.src} 
          width={this.width} height={this.height}></iframe>
      </Host>
    );
  }

}
