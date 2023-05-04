
import { PrecacheController } from 'workbox-precaching';
import { Route, RegExpRoute, NavigationRoute, Router } from 'workbox-routing';
import { NetworkOnly, CacheFirst, StaleWhileRevalidate } from 'workbox-strategies';
import { setCacheNameDetails } from 'workbox-core';

declare const self: ServiceWorkerGlobalScope;
declare const __WB_MANIFEST: any[];

setCacheNameDetails({
    prefix: "ufe-web-ui",
    suffix: "v1",
    precache: "precache",
});

const precacheAssets = []
precacheAssets.push( ...self.__WB_MANIFEST);

const precacheController = new PrecacheController();
precacheController.addToCacheList(precacheAssets);

const router = new Router();
router.setDefaultHandler(new NetworkOnly());
router.registerRoute(new Route(
    ({ url }) => url.pathname.endsWith('/fe-config'), new StaleWhileRevalidate(), "GET"));
router.registerRoute(new RegExpRoute(/\/api\//, new NetworkOnly(), "GET"));
router.registerRoute(new NavigationRoute(new StaleWhileRevalidate()));
router.registerRoute(new RegExpRoute(/\/webcomponents\//, new CacheFirst()));
router.registerRoute(new RegExpRoute(/.*/, new CacheFirst(), "GET"));

self.addEventListener("install", (event: any) => {
    event.waitUntil(precacheController.install(event));
});

self.addEventListener('activate', (event: any) => {
    event.waitUntil(precacheController.activate(event));
});


self.addEventListener('fetch', async (event: any) => {
    const { request } = event;
    
    if( request.method !== "GET" ||
        (request.url as string).includes('/api/') ||
        ( request.hostname && request.hostname !== self.location.hostname)
    ) { return ; }
    
    console.log(`v55 ${request.url}: ${request.mode} }`);
    return event.respondWith(router.handleRequest({
        event,
        request,
    }));
});

 