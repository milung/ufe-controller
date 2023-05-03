import { Config } from '@stencil/core';

// https://stenciljs.com/docs/config

export const config: Config = {
  globalStyle: 'src/global/app.css',
  globalScript: 'src/global/app.ts',
  taskQueue: 'async',
  sourceMap: true,
  
  outputTargets: [
    {
      type: 'www',
      buildDir: 'modules',
      dir: '../controller/www',
      serviceWorker: null,
      copy: [
        { src: 'manifest.template.json', dest: 'manifest.template.json' }
      ]
    },
  ],
};
