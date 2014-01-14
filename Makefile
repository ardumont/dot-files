pull:
	git pull --rebase --prune origin master

push:
	git push origin master

upg: pull
	cd ~/bin/ && make pull
	cd ~/org/ && make pull
