import { newSpecPage } from '@stencil/core/testing';
import { UfeFrame } from '../ufe-frame';

describe('ufe-frame', () => {
  it('renders', async () => {
    const page = await newSpecPage({
      components: [UfeFrame],
      html: `<ufe-frame></ufe-frame>`,
    });
    expect(page.root).toEqualHtml(`
      <ufe-frame>
        <mock:shadow-root>
          <slot></slot>
        </mock:shadow-root>
      </ufe-frame>
    `);
  });
});
