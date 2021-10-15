import { Config } from '@stencil/core';

export const config: Config = {
  namespace: 'profile-ui',
  outputTargets: [
    {
      type: 'dist',
      // esmLoaderPath: '../loader',
    },   
    {
      type: 'www',
      serviceWorker: null, // disable service workers
    },
  ],
};
