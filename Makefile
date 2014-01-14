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
