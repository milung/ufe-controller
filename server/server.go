package server

import (
	"html/template"
	"log"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/SevcikMichal/microfrontends-webui/configuration"
	"github.com/SevcikMichal/microfrontends-webui/model"
)

func ServeSinglePageApplication(w http.ResponseWriter, r *http.Request) {
	language, _ := requestMatchLanguage(r, configuration.GetAcceptsLanguages())

	htmlToFind := "index." + language + ".html"
	matches := getAllPossibleFiles(htmlToFind)
	bestFit := getFirstMatchingFile("web-ui/www", matches)

	data, err := os.ReadFile(bestFit)
	if err != nil {
		if os.IsNotExist(err) {
			log.Println("index.html does not exist!")
			http.NotFound(w, r)
			return
		}
		log.Panic(err)
	}

	fileContents := string(data)

	tmpl, err := template.New("index.html").Parse(fileContents)
	if err != nil {
		log.Panic(err)
	}

	pageData := &model.TemplateData{
		Language:                   "en",
		AppTitle:                   "Micro Frontends WebUI",
		BaseURL:                    configuration.GetBaseURL(),
		Description:                "Micro Frontends WebUI",
		MicroFrontendShellContext:  "application-shell",
		MicroFrontendSelector:      "",
		ProgresiveWebAppMode:       "false",
		ContentSecurityPolicyNonce: "123",
		TouchIcon:                  "",
		FavIcon:                    "",
	}

	err = tmpl.Execute(w, pageData)
	if err != nil {
		log.Panic(err)
	}
}

func ServeFile(w http.ResponseWriter, r *http.Request) {
	fileName := strings.Split(r.URL.Path, "/")[len(strings.Split(r.URL.Path, "/"))-1]

	matches := getAllPossibleFiles(fileName)
	bestFit := getFirstMatchingFile("web-ui/www", matches)

	file, err := os.Open(bestFit)
	if err != nil {
		if os.IsNotExist(err) {
			log.Println("index.html does not exist!")
			http.NotFound(w, r)
			return
		}
		log.Panic(err)
	}

	http.ServeContent(w, r, matches[0], time.Now(), file)
}

func ServeManifestJson(w http.ResponseWriter, r *http.Request) {
	language, _ := requestMatchLanguage(r, configuration.GetAcceptsLanguages())
	htmlToFind := "index." + language + ".html"
	matches := getAllPossibleFiles(htmlToFind)
	bestFit := getFirstMatchingFile("web-ui/www", matches)

	data, err := os.ReadFile(bestFit)

	if err != nil {
		if os.IsNotExist(err) {
			log.Println("manifest.template.json does not exist!")
			http.NotFound(w, r)
			return
		}
		log.Panic(err)
	}

	fileContents := string(data)

	tmpl, err := template.New("manifest.json").Parse(fileContents)
	if err != nil {
		log.Panic(err)
	}

	pageData := &model.TemplateData{
		AppTitle:        "Micro Frontends WebUI",
		AppTitleShort:   "µFE",
		BaseURL:         configuration.GetBaseURL(),
		AppIconLarge:    "",
		AppIconSmall:    "",
		TouchIcon:       "",
		BackgroundColor: "#ffffff",
		ThemeColor:      "#ffffff",
	}

	err = tmpl.Execute(w, pageData)
	if err != nil {
		log.Panic(err)
	}
}
