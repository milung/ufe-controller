import { Component,  h } from '@stencil/core';


@Component({
  tag: 'app-profile',
  styleUrl: 'app-profile.css',
  shadow: true,
})
export class AppProfile {
  // @Prop() match: MatchResults;

  normalize(name: string): string {
    if (name) {
      return name.substr(0, 1).toUpperCase() + name.substr(1).toLowerCase();
    }
    return '';
  }

  render() {
    // if (this.match && this.match.params.name) {
      return (
        <div class="app-profile">
          <p>Hello! My name is {this.normalize("match")}. My name was passed in through a route param!</p>
        </div>
      );
    // }
  }
}
