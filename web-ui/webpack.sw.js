const path = require('path');
const webpack = require('webpack');
const workbox_build = require('workbox-build');

const config = {
    mode: 'developement',
    target: 'webworker',
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
        return { sw: './dist/sw.ts' }

    },
    output: {
        path: path.resolve(__dirname, '../controller/www/modules'),
        filename: 'sw.mjs',
    },

    plugins: [
        new webpack.IgnorePlugin({ resourceRegExp: /^\.\/fe-config.mjs$/}), // injected at runtinme
    ],

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

module.exports = (_env, argv) => {
    if (argv.mode === 'production') {
        config.mode = 'production';
    } else {
        config.devtool = 'source-map';
    }

    return config;
};
