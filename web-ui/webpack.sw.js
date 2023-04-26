// Generated using webpack-cli https://github.com/webpack/webpack-cli

const path = require('path');
const workbox_build = require('workbox-build');

const config = {
    mode: 'production',
    entry: () => {
        workbox_build.injectManifest({
            swSrc: './src/service_worker/sw.ts',
            swDest: './dist/sw.ts',
            globDirectory: '../controller/www/',
            globPatterns: [
              '**/*.js',
              '**/*.ico',
              '**/*.css',
              '**/*.html',
              '**/*.svg', 
              '**/*.png'
            ],
          });
        return './dist/sw.ts'
    },
    output: {
        path: path.resolve(__dirname, '../controller/www/modules'),
        filename: 'sw.mjs',
    },
    
    module: {
        rules: [
            {
                test: /\.(ts|tsx)$/i,
                loader: 'ts-loader',
                exclude: ['/node_modules/'],
            },
            {
                test: /\.(eot|svg|ttf|woff|woff2|png|jpg|gif)$/i,
                type: 'asset',
            },
        ],
    },
    resolve: {
        extensions: ['.tsx', '.ts', '.jsx', '.js', '...'],
    },
};

module.exports = () => {return config;};
