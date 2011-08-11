.PHONY: all html

FILES = hydrodoc_intro.txt \
        hydrodoc_exceptions.txt \
        hydrodoc_classes.txt \
        hydrodoc_proxies.txt \
        hydrodoc_servers.txt \
        hydrodoc_meta.txt \
        hydrodoc_hydromon.txt

all: html

html:
	cd ../src/hydro && omake ocamldoc.dump
	cd ../src/hydromon && omake ocamldoc.dump
	rm -rf html
	mkdir -p html
	cp style.css html
	ocamldoc -d html \
		-load ../src/hydro/ocamldoc.dump \
		-load ../src/hydromon/ocamldoc.dump \
	        $(FILES) \
		-t "Hydro: An implementation of the Ice protocol" \
                -intro index.txt \
		-html -css-style style.css
