TARGET_DIR ?=/usr/local/lib
RC_DIR ?=/usr/local/etc/rc.d
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

$(STAGE_DIR)/+MANIFEST: +MANIFEST.in
	sed -e 's/__VNS__/${VERSION}${SUFFIX}/' +MANIFEST.in > $(STAGE_DIR)/+MANIFEST
	( echo 'deps:  { '; \
		for dep in ${DEPS}; do \
		pkg query --glob "	\"%n\" : { \"origin\" : \"%o\", \"version\" : \"%v\" }," "$$dep"; \
		done ; \
		echo '}' ) >> $(STAGE_DIR)/+MANIFEST

clean-pkg:

clean: clean-pkg
