
import { PrecacheController } from 'workbox-precaching';
import { Route, RegExpRoute, NavigationRoute, Router } from 'workbox-routing';
import { NetworkOnly, CacheFirst, StaleWhileRevalidate } from 'workbox-strategies';

declare const self: ServiceWorkerGlobalScope;
declare const __WB_MANIFEST: any[];

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
router.registerRoute(new RegExpRoute(/.*/, new CacheFirst(), "GET"));

self.addEventListener("install", (event: any) => {
    event.waitUntil(precacheController.install(event));
});

self.addEventListener('activate', (event: any) => {
    event.waitUntil(precacheController.activate(event));
});

self.addEventListener('fetch', async (event: any) => {
    const { request } = event;
    const responsePromise = router.handleRequest({
        event,
        request,
    });
    if (responsePromise) {
        event.respondWith(responsePromise);
    } else {
        event.respondWith(await fetch(request))
    }
});

