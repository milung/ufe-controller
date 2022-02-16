const http = require('http');
const httpProxy = require('http-proxy');
const { runInNewContext } = require('vm');


const proxyApi = new httpProxy.createProxyServer({
    target: {
        host: '127.0.0.1',
        port: 5252
    },
});

const proxyStencil = new httpProxy.createProxyServer({
    target: {
        host: '127.0.0.1',
        port: 3333
    }
});


const proxyServer = http.createServer(function (req, res) {
    console.log(req.url);
    if ( req.url.match(/\/fe-config/) 
        ||  req.url.match(/\/web-components/)
        ||  req.url.match(/\/app-icons/)
        ||  req.url.match(/\/avatar/)) {
        console.log('api');
        proxyApi.web(req, res);
    } else {
        console.log('stencil 3');
        proxyStencil.web(req, res);
    }
});

//
// Listen to the `upgrade` event and proxy the
// WebSocket requests as well.
//
proxyServer.on('upgrade', function (req, socket, head) {
    proxyStencil.ws(req, socket, head);
});

console.log('Listen: http://127.0.0.1:5000');
proxyServer.listen(5000);