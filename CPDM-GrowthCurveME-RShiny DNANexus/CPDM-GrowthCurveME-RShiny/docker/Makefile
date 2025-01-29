TAG = growthcurveme:v0.0.1

img:
	docker build --tag=$(TAG) .

shell:
	docker run --rm -it $(TAG) bash

save:
	docker save $(TAG) | gzip > $(shell echo $(TAG) | sed "s/:/-/").tar.gz
