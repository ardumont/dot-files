REPO_DOTFILES=~/repo/perso/dot-files
WITH_EMACS_ENV="prelude"
CI=n

deploy:
	$(REPO_DOTFILES)/deploy.sh $(WITH_EMACS_ENV) $(CI)

deploy-keysnail:
	$(REPO_DOTFILES)/deploy-keysnail.sh

pull: pull-dot pull-bin pull-org pull-emacs pull-packs

push: push-dot push-bin push-org push-emacs push-packs

pull-dot:
	cd $(REPO_DOTFILES) && git pull --rebase --prune origin master

pull-bin:
	cd ~/bin/ && make pull

pull-org:
	cd ~/org/ && make pull

pull-emacs:
	cd ~/.emacs.d && make pull

pull-packs:
	cd ~/.prelude-packs && make pull

push-dot:
	cd $(REPO_DOTFILES) && git push origin master

push-bin:
	cd ~/bin/ && make push

push-org:
	cd ~/org/ && make push

push-emacs:
	cd ~/.emacs.d && make push

push-packs:
	cd ~/.prelude-packs/ && make push

test: deploy
