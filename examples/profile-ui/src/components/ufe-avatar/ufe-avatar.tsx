import { Component, Prop, h } from '@stencil/core';
import md5 from "md5";

@Component({
  tag: 'ufe-avatar',
  styleUrl: 'ufe-avatar.css',
  shadow: true,
})
export class Avatar {
  /**
   * The first name
   */
  @Prop() email: string;

  /**
   * The middle name
   */
  @Prop() size: number;


  render() {
    const hash = md5(this.email.toLowerCase());
    const url = `https://seccdn.libravatar.org/avatar/${hash}`;

    return <img src={url} alt="" loading="lazy"/>;
  }
}
