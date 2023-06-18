package main

import (
	"log"
	"net/http"

	"github.com/SevcikMichal/microfrontends-webui/router"
)

func main() {
	log.Println("Main function starting...")
	startHTTPServer()
}

func startHTTPServer() {
	router := router.CreateRouter()

	server := &http.Server{
		Addr:    ":8082",
		Handler: router,
	}

	if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatal(err, "problem running server")
	}
}
