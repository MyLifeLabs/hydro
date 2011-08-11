VERSION=0.7.1

.PHONY: all opt clean install uninstall VERSION release

opt: VERSION
	cd src/hydrogen; omake NATIVE_ENABLED=true
	cd src/hydro; omake NATIVE_ENABLED=true
	cd src/hydromon; omake NATIVE_ENABLED=true

all: VERSION
	cd src/hydrogen; omake NATIVE_ENABLED=false
	cd src/hydro; omake NATIVE_ENABLED=false
	cd src/hydromon; omake NATIVE_ENABLED=false

clean: VERSION
	cd src/hydrogen; omake clean
	cd src/hydro; omake clean
	cd src/hydromon; omake clean

install: VERSION
	cd src/hydrogen; omake install
	cd src/hydro; omake install
	cd src/hydromon; omake install

uninstall: VERSION
	cd src/hydrogen; omake uninstall
	cd src/hydro; omake uninstall
	cd src/hydromon; omake uninstall

VERSION:
	echo "$(VERSION)" >VERSION

RFILES=	\
	LICENSE \
	README \
	Makefile \
	src/OMakeroot \
	src/OMakefile \
	src/hydrogen/META.in \
	src/hydrogen/OMakefile \
	src/hydrogen/hgen_analyzer.ml \
	src/hydrogen/hgen_lexer.mll \
	src/hydrogen/hgen_lexer_driver.ml \
	src/hydrogen/hgen_main.ml \
	src/hydrogen/hgen_parser.mly \
	src/hydrogen/hgen_parser_util.ml \
	src/hydrogen/hgen_print_il.ml \
	src/hydrogen/hgen_simplif.ml \
	src/hydrogen/hgen_trans_il.ml \
	src/hydrogen/hgen_types.ml \
	src/hydrogen/hgen_util.ml \
	src/hydro/META.in \
	src/hydro/OMakefile \
	src/hydro/hydro_builtin.ice \
	src/hydro/hydro_builtin_util.ml \
	src/hydro/hydro_connector.ml \
	src/hydro/hydro_connector.mli \
	src/hydro/hydro_dbg.ml \
	src/hydro/hydro_endpoint.ml \
	src/hydro/hydro_endpoint.mli \
	src/hydro/hydro_lexer.mll \
	src/hydro/hydro_lm.ml \
	src/hydro/hydro_lm.mli \
	src/hydro/hydro_lm_IceLocalObject.ml \
	src/hydro/hydro_lm_IceLocalObject.mli \
	src/hydro/hydro_lm_IceObject.ml \
	src/hydro/hydro_lm_IceObject.mli \
	src/hydro/hydro_locator.ml \
	src/hydro/hydro_locator.mli \
	src/hydro/hydro_marshal.ml \
	src/hydro/hydro_marshal.mli \
	src/hydro/hydro_message.ml \
	src/hydro/hydro_message.mli \
	src/hydro/hydro_netplex.ml \
	src/hydro/hydro_netplex.mli \
	src/hydro/hydro_oa.ml \
	src/hydro/hydro_oa.mli \
	src/hydro/hydro_params.ml \
	src/hydro/hydro_params.mli \
	src/hydro/hydro_prelim.ml \
	src/hydro/hydro_prelim.mli \
	src/hydro/hydro_proxy.ml \
	src/hydro/hydro_proxy.mli \
	src/hydro/hydro_string.ml \
	src/hydro/hydro_string.mli \
	src/hydro/hydro_transport.ml \
	src/hydro/hydro_transport.mli \
	src/hydro/hydro_types.mli \
	src/hydro/hydro_unmarshal.ml \
	src/hydro/hydro_unmarshal.mli \
	src/hydro/hydro_util.ml \
	src/hydro/word_size.ml \
	src/hydromon/META.in \
	src/hydromon/OMakefile \
	src/hydromon/hydromon_netplex.ml \
	src/hydromon/hydromon_netplex.mli \
	src/hydromon/hydromon_proto.ice \
	src/hydromon/hydromon_query.ml \
	src/hydromon/hydromon_query.mli \
	src/hydromon/hydromon_test.cfg \
	src/hydromon/hydromon_test.ml \
	src/hydromon/hydromon_util.ml

.PHONY: release

release:
	if [ ! -d doc/html ]; then echo "No documentation"; exit 1; fi
	rm -rf release/hydro-$(VERSION)
	mkdir -p release/hydro-$(VERSION)
	mkdir -p release/hydro-$(VERSION)/doc
	mkdir -p release/hydro-$(VERSION)/doc/html
	find $(RFILES) | cpio -pvd release/hydro-$(VERSION)
	cp doc/html/* release/hydro-$(VERSION)/doc/html
	cd release && tar czf hydro-$(VERSION).tar.gz hydro-$(VERSION)
