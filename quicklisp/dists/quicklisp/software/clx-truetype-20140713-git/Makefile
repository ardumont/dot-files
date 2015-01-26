.PHONY: web

web:
	rm -rf web
	mkdir web
	make -C doc html pdf
	cp doc/*.html doc/*.pdf doc/*.png web/
	cp web/clx-truetype.html web/index.html

pages: web
	git checkout gh-pages
	cp web/* .
	git commit -a -c master
	rm -rf web/
	git checkout -f master
