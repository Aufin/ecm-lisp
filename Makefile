podman := podman
local_tag := ecm:latest
hub_tag := docker.io/aufin/ecm:latest
run_tag := $(local_tag)
yaml_name := ecm-app-deployment.yaml


build:
	$(podman) build -t $(local_tag) .

run:
	$(podman) run -ti --add-host=ecm-cluster-rw:host-gateway -p "0.0.0.0:8443:443" $(run_tag)

push:
	$(podman) push $(local_tag) $(hub_tag)

yaml:
	$(podman) pod create --replace --name ecm-app 
	$(podman) create --pod ecm-app --name ecm-appd --replace docker.io/aufin/ecm:latest
	$(podman) kube generate --type deployment --replicas 2 -f $(yaml_name) ecm-app
