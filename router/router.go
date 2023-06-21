package router

import (
	"github.com/SevcikMichal/microfrontends-webui/configuration"
	"github.com/SevcikMichal/microfrontends-webui/server"
	"github.com/gorilla/mux"
)

func CreateRouter() *mux.Router {
	router := mux.NewRouter().StrictSlash(true)
	basePathRouter := router.PathPrefix(configuration.GetBaseURL()).Subrouter()

	// manifest.json
	basePathRouter.HandleFunc("/manifest.json", server.ServeManifestJson).Methods("GET")

	//service-worker.js
	basePathRouter.HandleFunc("/sw.mjs", server.ServeFile).Methods("GET")

	// modules, fonts, assets, favicon.ico
	basePathRouter.PathPrefix("/modules").HandlerFunc(server.ServeFile).Methods("GET")
	basePathRouter.PathPrefix("/assets").HandlerFunc(server.ServeFile).Methods("GET")
	basePathRouter.PathPrefix("/fonts").HandlerFunc(server.ServeFile).Methods("GET")
	basePathRouter.PathPrefix("/favicon.ico").HandlerFunc(server.ServeFile).Methods("GET")

	// index.html
	basePathRouter.PathPrefix("/").HandlerFunc(server.ServeSinglePageApplication).Methods("GET")

	return router
}
