package server

import (
	"fmt"
	"net/http"
	"sort"
	"strconv"
	"strings"

	"github.com/SevcikMichal/microfrontends-webui/model"
)

func requestMatchLanguage(request *http.Request, supported []string) (string, error) {
	accepts := requestAcceptLanguages(request, map[string]interface{}{})
	language, err := languagesSupportedMatch(accepts, supported)
	if err != nil {
		return "", err
	}
	return language, nil
}

func requestAcceptLanguages(request *http.Request, options map[string]interface{}) []string {
	// Check if the lang parameter is specified as a query parameter
	languages := []string{}
	queries := request.URL.Query()
	for key, value := range queries {
		if key == "lang" {
			userLang := value
			userNorm := languageNormalized(userLang, options)
			languages = append(languages, userNorm...)
		}
	}

	if acceptLanguages := request.Header.Values("Accept-Language"); len(acceptLanguages) > 0 {
		acceptLanguages = strings.Split(acceptLanguages[0], ",")
		unsortedLanguages := make([]model.LanguagePreference, len(acceptLanguages))

		for i, lang := range acceptLanguages {
			unsortedLanguages[i] = languagePreference(lang, options)
		}

		sort.SliceStable(unsortedLanguages, func(i, j int) bool {
			return unsortedLanguages[i].Quality > unsortedLanguages[j].Quality
		})

		languages := make([]string, len(unsortedLanguages))
		for i, language := range unsortedLanguages {
			languages[i] = language.Language
		}

		return languages
	}

	// Return the default language if no Accept-Language header is specified
	if noHeader, ok := options["no_header"]; ok && noHeader == "fail" {
		panic("Accept-Language header not specified")
	}

	if defaultLang, ok := options["no_header"]; ok {
		return languageNormalized([]string{defaultLang.(string)}, options)
	}

	// Return the default language "en" if no options are specified
	return []string{"en"}
}

func languageNormalized(languages []string, options map[string]interface{}) []string {
	langs := []string{}
	for _, language := range languages {
		if language == "*" {
			if wildcard, ok := options["wildcard_value"].(string); ok {
				return []string{wildcard}
			}
			return []string{"*"}
		}

		lang := strings.ToLower(language)
		lang = strings.ReplaceAll(lang, "-", "_")
		langs = append(langs, lang)
	}
	return langs
}

func languagePreference(acceptedLanguage string, options map[string]interface{}) model.LanguagePreference {
	if splited := strings.Split(acceptedLanguage, ";"); len(splited) > 1 {
		language, quality := splited[0], splited[1]
		qualityValue := extractQualityValue(quality)
		lang := languageNormalized([]string{language}, options)
		return model.LanguagePreference{Language: lang[0], Quality: qualityValue}
	} else {
		return model.LanguagePreference{Language: acceptedLanguage, Quality: 1.0}
	}
}

func extractQualityValue(quality string) float64 {
	qualityString := quality[2:]
	qualityValue, _ := strconv.ParseFloat(qualityString, 64)
	return qualityValue
}

func languagesSupportedMatch(preferences []string, systemLanguages []string) (string, error) {
	if len(preferences) == 0 {
		if len(systemLanguages) > 0 {
			return systemLanguages[0], nil
		}
		return "", fmt.Errorf("no supported language found")
	}

	lang := preferences[0]
	if containsString(systemLanguages, lang) {
		return lang, nil
	}

	return languagesSupportedMatch(preferences[1:], systemLanguages)
}

func containsString(slice []string, str string) bool {
	for _, s := range slice {
		if s == str {
			return true
		}
	}
	return false
}
