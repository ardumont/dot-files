REPO_DOTFILES=~/repo/perso/dot-files
REPO_BIN=~/bin
REPO_ORG=~/org
REPO_EMACS=~/.emacs.d
REPO_PACKS=~/.prelude-packs
WITH_EMACS_ENV="prelude"
CI=n
DISPLAY=:0.0

deploy:
	$(REPO_DOTFILES)/deploy.sh $(WITH_EMACS_ENV) $(CI)

deploy-keysnail:
	$(REPO_DOTFILES)/deploy-keysnail.sh

pull: pull-dot pull-bin pull-org pull-packs

push: push-dot push-bin push-org push-packs

pull-dot:
	cd $(REPO_DOTFILES) && git pull --rebase --prune origin master

pull-bin:
	cd $(REPO_BIN) && make pull

pull-org:
	cd $(REPO_ORG) && make pull

pull-emacs:
	cd $(REPO_EMACS) && make pull

pull-packs:
	cd $(REPO_PACKS) && make pull

push-dot:
	cd $(REPO_DOTFILES) && git push origin master

push-bin:
	cd $(REPO_BIN) && make push

push-org:
	cd $(REPO_ORG) && make push

push-emacs:
	cd $(REPO_EMACS) && make push

push-packs:
	cd $(REPO_PACKS) && make push

test: deploy

evolution:
	xhost + && \
	docker run -it \
	  -v $(HOME):/home/user \
	  -v /tmp/.X11-unix:/tmp/.X11-unix \
	  -e DISPLAY=unix$(DISPLAY) \
	   ardumont/evolution

soapui:
	xhost + && \
	  docker run -it \
	  -v /tmp/.X11-unix:/tmp/.X11-unix \
	  -e DISPLAY=unix$(DISPLAY) \
	  ardumont/ubuntu-soapui:run

nixos-rebuild:
	sudo nixos-rebuild switch -I nixpkgs=$(HOME)/repo/perso/nixpkgs

reboot:
	sudo systemctl kexec

shutdown:
	sudo systemctl poweroff

suspend:

mount-samba:
	mount -t cifs -o username=adumont //ibiza/commun2 ~/share/ibiza/
	sudo systemctl suspend
