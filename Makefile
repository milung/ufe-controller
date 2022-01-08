############ PRESETS ##################

# DEPLOYMENT
DEPLOY_ENVIRONMENT=sample
DEPLOY_CONTEXT=wac
DEPLOY_NAMESPACE=micro-fe-sample

# DIRECTORIES
BUILD_DIR=.build

# IMAGES 
CONTROLLER_TAG=milung/ufe-controller
PROFILES_UIC_TAG=milung/profile-uic


##########  GOALS #####################

.PHONY: deploy

controller.push: controller.image
	docker push $(CONTROLLER_TAG)
	@docker inspect --format='{{.ID}}' $(CONTROLLER_TAG) > .build/controller.push

controller.image:  Dockerfile $(wildcard controller/sources/**/*.pl) $(wildcard controller/*.pl) $(wildcard controller/.packages/**/prolog/*.pl) | $(BUILD_DIR)
	docker build -t $(CONTROLLER_TAG) .
	@docker inspect --format='{{.ID}}' $(CONTROLLER_TAG) > .build/controller.image

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
 MAKE_PATH = $(POWERSHELL)  $$null = new-item -ItemType "directory" -Force -Path
else
 MAKE_PATH = mkdir -p 	 
endif