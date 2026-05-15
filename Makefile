.PHONY: clean vendor

VENDOR_DIR := lib/3rdparty
CACHE_DIR := $(VENDOR_DIR)/.cache

# just in case I need to template it later for some reason
define GITIGNORE_CONTENTS
.cache/
endef

# syntax: <URL>|<name>
# names must be unique
# URLs are expected to be .tar.gz files with an additional directory wrapping stuff
PACKAGES :=

$(CACHE_DIR): $(VENDOR_DIR)/.gitignore | $(VENDOR_DIR)
	mkdir -p $(CACHE_DIR)

$(VENDOR_DIR):
	mkdir -p $(VENDOR_DIR)

$(VENDOR_DIR)/.gitignore: | $(VENDOR_DIR)
	$(file > $(VENDOR_DIR)/.gitignore,$(GITIGNORE_CONTENTS))

clean:
	rm -rfv $(VENDOR_DIR)

define process_spec_template
$(let PKG_URL PKG_NAME, $(subst |, ,$(1)),$$(VENDOR_DIR)/$(PKG_NAME): $$(CACHE_DIR)/$(PKG_NAME).tar.gz | $$(VENDOR_DIR)
	TMPDIR=$$(shell mktemp -d -p $$(CACHE_DIR)) \
	&& tar -C $$$$TMPDIR -xzvf $$(CACHE_DIR)/$(PKG_NAME).tar.gz --strip-components=1 \
	&& mv -v $$$$TMPDIR $$(VENDOR_DIR)/$(PKG_NAME)
$$(CACHE_DIR)/$(PKG_NAME).tar.gz: | $$(CACHE_DIR)
	curl -L $(PKG_URL) -o $$(CACHE_DIR)/$(PKG_NAME).tar.gz.partial --remove-on-error \
	&& mv -v $$(CACHE_DIR)/$(PKG_NAME).tar.gz.partial $$(CACHE_DIR)/$(PKG_NAME).tar.gz\
)
endef

# generate rules for spec templates
$(foreach SPEC,$(PACKAGES),$(eval $(call process_spec_template,$(SPEC))))

# these are primary installation targets
INSTALLED = $(foreach SPEC,$(PACKAGES),$(VENDOR_DIR)/$(lastword $(subst |, ,$(SPEC))))

vendor: | $(INSTALLED)
