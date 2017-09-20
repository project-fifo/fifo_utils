TARGET_DIR ?=/usr/local/lib
RC_DIR ?=/usr/local/etc/rc.d
FILE ?=$(COMPONENT)-$(VERSION)$(SUFFIX)
BLOCK_SIZE ?=65536
STAGE_DIR ?=deploy
TMP_DIR ?=tmp
PKG_CATEGORY ?=fifo
PKG_HOMEPAGE ?=https://project-fifo.net

.PHONY: package_list dep_list clean-pkg

package: prepare $(STAGE_DIR)/+MANIFEST $(STAGE_DIR)/plist
	cp +POST_INSTALL $(STAGE_DIR)/
	pkg create -m $(STAGE_DIR)/ -r $(STAGE_DIR)/ -p $(STAGE_DIR)/plist -o .

$(STAGE_DIR)/plist:
	-rm $(STAGE_DIR)/plist || true
	(cd $(STAGE_DIR); find * -type f | sort | grep -v '^+' | grep -v '^plist$$') >> $(STAGE_DIR)/plist

clean-pkg:

$(STAGE_DIR)/+MANIFEST: +MANIFEST.in
	sed -e 's/__VNS__/${VERSION}/' +MANIFEST.in > $(STAGE_DIR)/+MANIFEST
	( echo 'deps:  { '; \
		for dep in ${DEPS}; do \
		pkg query --glob "	\"%n\" : { \"origin\" : \"%o\", \"version\" : \"%v\" }," "$$dep"; \
		done ; \
		echo '}' ) >> $(STAGE_DIR)/+MANIFEST

tmp/$(FILE).tgz: dep_list package_list +MANIFEST
	-rm -r tmp
	mkdir tmp
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I $(TARGET_DIR) -p $(STAGE_DIR) -U tmp/$(FILE).tgz

clean: clean-pkg
	-rm -r tmp build-info packlist
	-rm -r $(STAGE_DIR)
	-rm *.tgz
