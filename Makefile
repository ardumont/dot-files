deploy:
	~/repo/perso/dot-files/deploy.sh

pull:
	git pull --rebase --prune origin master

push:
	git push origin master

upg: pull
	cd ~/bin/ && make pull
	cd ~/org/ && make pull
	cd ~/.emacs.d && make pull
	cd ~/.emacs-live-packs && make pull

pupg: push
	cd ~/bin/ && make push
	cd ~/org/ && make push
	cd ~/.emacs.d && make push
	cd ~/.emacs-live-packs && make push
