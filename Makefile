############ PRESETS ##################
# VERSION INFORMATION
VERSION=0.1

# DEPLOYMENT
DEPLOY_ENVIRONMENT=sample
DEPLOY_CONTEXT=wac
DEPLOY_NAMESPACE=micro-fe-sample

# DIRECTORIES
BUILD_DIR=.build

# IMAGES 
CONTROLLER_TAG=milung/ufe-controller
PROFILES_UIC_TAG=milung/profile-uic

# SOURCES

rwildcard=$(foreach d,$(wildcard $(1:=/*)),$(call rwildcard,$d,$2) $(filter $(subst *,%,$2),$d))
WEB_SOURCES=$(call rwildcard, web-ui/src, *) web-ui/package.json web-ui/stencil.config.ts
CONTROLLER_SOURCES=$(call rwildcard, controller, *.pl)


##########  GOALS #####################

.PHONY: deploy

controller.push: controller.image
	docker push $(CONTROLLER_TAG)
	@docker inspect --format='{{.ID}}' $(CONTROLLER_TAG) > .build/controller.push

controller.image:  Dockerfile $(CONTROLLER_SOURCES) $(WEB_SOURCES) | $(BUILD_DIR)
	docker build \
		-t $(CONTROLLER_TAG) \
		--build-arg BUILD_ENV=build.prod \
		--label "version=$(VERSION)"  \
		--label "build-mode=production"  \
		--label "build-timestamp=$(TIMESTAMP)" \
		.
	@docker inspect --format='{{.ID}}' $(CONTROLLER_TAG) > .build/controller.image

dev.controller.push: dev.controller.image
	docker push $(CONTROLLER_TAG):dev
	@docker inspect --format='{{.ID}}' $(CONTROLLER_TAG) > .build/dev.controller.push

dev.controller.image: Dockerfile $(CONTROLLER_SOURCES) $(WEB_SOURCES) | $(BUILD_DIR)
	docker build -t $(CONTROLLER_TAG):dev --build-arg BUILD_ENV=build.dev .
	@docker inspect --format='{{.ID}}' $(CONTROLLER_TAG):dev > .build/dev.controller.image

profiles.push: profiles.image
	docker push $(PROFILES_UIC_TAG)
	@docker inspect --format='{{.ID}}' $(PROFILES_UIC_TAG) > .build/profiles.push

profiles.image:
	docker build -t $(PROFILES_UIC_TAG) examples/profile-ui
	@docker inspect --format='{{.ID}}' $(PROFILES_UIC_TAG) > .build/profiles.image

deploy: controller.push profiles.push
	$(KUSTOMIZE) build kustomize/environments/overlays/$(DEPLOY_ENVIRONMENT) | $(KUBECTL) apply -f -
	

.build:
	@$(MAKE_PATH)  .build


vpath %.image .build
vpath %.push .build
vpath %.pl controller
vpath %.pl controller/sources
vpath %.pl controller/.packages

############## TOOLS ##################

KUSTOMIZE=kustomize
KUBECTL=kubectl --context $(DEPLOY_CONTEXT) --namespace $(DEPLOY_NAMESPACE) 
POWERSHELL = pwsh -noprofile -command 


ifeq ($(OS),Windows_NT)
 MAKE_PATH = $(POWERSHELL)  $$null = new-item -ItemType "directory" -Force -Path)
 TIMESTAMP=  $(shell $(POWERSHELL) "Get-Date (Get-Date).ToUniversalTime() -UFormat '+%Y-%m-%dT%H:%M:%SZ'" )
else
 MAKE_PATH = mkdir -p 
 TIMESTAMP = $(shell date --iso=seconds)
endif