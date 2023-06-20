package router

import (
	"github.com/SevcikMichal/microfrontends-webui/server"
	"github.com/gorilla/mux"
)

func CreateRouter() *mux.Router {
	router := mux.NewRouter().StrictSlash(true)
	// basePathRouter := router.PathPrefix(configuration.GetBaseURL()).Subrouter()

	// index.html
	router.HandleFunc("/", server.ServeSinglePageApplication).Methods("GET")

	// manifest.json
	router.HandleFunc("/manifest.json", server.ServeManifestJson).Methods("GET")

	//service-worker.js
	router.HandleFunc("/sw.mjs", server.ServeFile).Methods("GET")

	// modules, fonts, assets, favicon.ico
	router.PathPrefix("/modules").HandlerFunc(server.ServeFile).Methods("GET")
	router.PathPrefix("/assets").HandlerFunc(server.ServeFile).Methods("GET")
	router.PathPrefix("/fonts").HandlerFunc(server.ServeFile).Methods("GET")
	router.PathPrefix("/favicon.ico").HandlerFunc(server.ServeFile).Methods("GET")

	router.PathPrefix("/").HandlerFunc(server.PassThrough).Methods("GET")

	return router
}
