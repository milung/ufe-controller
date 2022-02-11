import { newE2EPage } from '@stencil/core/testing';

describe('ufe-frame', () => {
  it('renders', async () => {
    const page = await newE2EPage();
    await page.setContent('<ufe-frame></ufe-frame>');

    const element = await page.find('ufe-frame');
    expect(element).toHaveClass('hydrated');
  });
});
