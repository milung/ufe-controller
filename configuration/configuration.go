package configuration

import (
	"os"
	"strings"
)

const (
	AcceptsLanguages           = "ACCEPTS_LANGUAGES"
	AppIconLarge               = "APP_ICON_LARGE"
	AppIconSmall               = "APP_ICON_SMALL"
	ApplicationDescription     = "APPLICATION_DESCRIPTION"
	ApplicationShellContext    = "APPLICATION_SHELL_CONTEXT"
	ApplicationTitleShort      = "APPLICATION_TITLE_SHORT"
	ApplicationTitle           = "APPLICATION_TITLE"
	BaseURL                    = "BASE_URL"
	FaviconIco                 = "FAVICON_ICO"
	ForcedRefreshPeriodSeconds = "FORCED_REFRESH_PERIOD_SECONDS"
	HttpCspHeader              = "HTTP_CSP_HEADER"
	HttpPort                   = "HTTP_PORT"
	ManifestTemplate           = "MANIFEST_TEMPLATE"
	ObserveNamespaces          = "OBSERVE_NAMESPACES"
	PwaMode                    = "PWA_MODE"
	ServiceWorker              = "SERVICE_WORKER"
	SwVersion                  = "SW_VERSION"
	SwSkipFetch                = "SW_SKIP_FETCH"
	TouchIcon                  = "TOUCH_ICON"
	UserIdHeader               = "USER_ID_HEADER"
	UserEmailHeader            = "USER_EMAIL_HEADER"
	UserNameHeader             = "USER_NAME_HEADER"
	UserRolesHeader            = "USER_ROLES_HEADER"
	WebcomponentsSelector      = "WEBCOMPONENTS_SELECTOR"
)

func GetAcceptsLanguages() string {
	return os.Getenv(AcceptsLanguages)
}

func GetAppIconLarge() string {
	return os.Getenv(AppIconLarge)
}

func GetAppIconSmall() string {
	return os.Getenv(AppIconSmall)
}

func GetApplicationDescription() string {
	return os.Getenv(ApplicationDescription)
}

func GetApplicationShellContext() string {
	return os.Getenv(ApplicationShellContext)
}

func GetApplicationTitleShort() string {
	return os.Getenv(ApplicationTitleShort)
}

func GetApplicationTitle() string {
	return os.Getenv(ApplicationTitle)
}

func GetBaseURL() string {
	value, ok := os.LookupEnv(BaseURL)

	if ok {
		return value
	}

	return "/"
}

func GetFaviconIco() string {
	return os.Getenv(FaviconIco)
}

func GetForcedRefreshPeriodSeconds() string {
	return os.Getenv(ForcedRefreshPeriodSeconds)
}

func GetHttpCspHeader() string {
	return os.Getenv(HttpCspHeader)
}

func GetHttpPort() string {
	value, ok := os.LookupEnv(HttpPort)

	if ok {
		return value
	}

	return "80"
}

func GetManifestTemplate() string {
	return os.Getenv(ManifestTemplate)
}

func GetObserveNamespaces() []string {
	value, ok := os.LookupEnv(ObserveNamespaces)

	if ok {
		return strings.Split(value, ",")
	}

	return []string{}
}

func GetPwaMode() string {
	return os.Getenv(PwaMode)
}

func GetServiceWorker() string {
	return os.Getenv(ServiceWorker)
}

func GetSwVersion() string {
	return os.Getenv(SwVersion)
}

func GetSwSkipFetch() string {
	return os.Getenv(SwSkipFetch)
}

func GetTouchIcon() string {
	return os.Getenv(TouchIcon)
}

func GetUserIdHeader() string {
	value, ok := os.LookupEnv(UserIdHeader)

	if ok {
		return value
	}

	return "x-forwarded-user"
}

func GetUserEmailHeader() string {
	value, ok := os.LookupEnv(UserEmailHeader)

	if ok {
		return value
	}

	return "x-forwarded-email"
}

func GetUserNameHeader() string {
	value, ok := os.LookupEnv(UserNameHeader)

	if ok {
		return value
	}

	return "x-forwarded-preferred-username"
}

func GetUserRolesHeader() string {
	value, ok := os.LookupEnv(UserRolesHeader)

	if ok {
		return value
	}

	return "x-forwarded-groups"
}

func GetWebcomponentsSelector() string {
	return os.Getenv(WebcomponentsSelector)
}
