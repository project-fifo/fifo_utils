
TARGET_DIR ?= /opt/local/$(COMPONENT)
FILE ?= $(COMPONENT)-$(VERSION)$(SUFFIX)
BLOCK_SIZE ?= 65536
STAGE_DIR ?= deploy
TMP_DIR ?= tmp
PKG_CATEGORY ?= fifo
PKG_HOMEPAGE ?= https://project-fifo.net

package_list:
	-rm packlist
	for dep in $(DEPS); do echo "@pkgdep $$dep" >> packlist; done
	(cd deploy; find * -type f | sort) >> packlist

build_info:
	pkg_info -X pkg_install | egrep '^(MACHINE_ARCH|OPSYS|OS_VERSION|PKGTOOLS_VERSION)' >build-info
	echo "CATEGORIES=$(PKG_CATEGORY)" >>build-info
	echo "HOMEPAGE=$(PKG_HOMEPAGE)" >>build-info

clean-pkg:
	-rm -r tmp *.tgz build-info packlist


$(TMP_DIR)/$(FILE).tgz: package_list build_info
	-rm -r $(TMP_DIR)
	mkdir $(TMP_DIR)
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I $(TARGET_DIR) -p ../../ -U $(TMP_DIR)/$(FILE).tgz

ifdef GPG_KEY
$(FILE).tgz: $(TMP_DIR)/$(FILE).tgz +PKG_HASH +PKG_GPG_SIGNATURE
	-rm $(FILE).tgz
	gar r $(FILE).tgz +PKG_HASH +PKG_GPG_SIGNATURE $(TMP_DIR)/$(FILE).tgz
	rm -r $(TMP_DIR)
else
$(FILE).tgz: $(TMP_DIR)/$(FILE).tgz
	cp $(TMP_DIR)/$(FILE).tgz $(FILE).tgz
	rm -r $(TMP_DIR)
endif


+PKG_HASH: $(TMP_DIR)/$(FILE).tgz
	split -b $(BLOCK_SIZE) tmp/$(FILE).tgz tmp/split
	echo "pkgsrc signature\n\nversion: 1\npkgname: $(FILE)" > +PKG_HASH
	echo "algorithm: SHA512\nblock size: $(BLOCK_SIZE)" >> +PKG_HASH
	echo "file size: $(shell ls -l tmp/$(FILE).tgz | cut -d' ' -f 5)\n" >> +PKG_HASH
	for file in tmp/split*; do digest -a sha512 $$file >> +PKG_HASH; done
	echo "end pkgsrc signature" >> +PKG_HASH

+PKG_GPG_SIGNATURE: +PKG_HASH
	-rm +PKG_GPG_SIGNATURE
	gpg --default-key $(GPG_KEY) -o +PKG_GPG_SIGNATURE --detach-sign +PKG_HASH
