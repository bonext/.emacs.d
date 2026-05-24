.PHONY: clean vendor

VENDOR_DIR := lib/3rdparty
CACHE_DIR := $(VENDOR_DIR)/.cache

# just in case I need to template it later for some reason
define GITIGNORE_CONTENTS
.cache/
**/*.png
**/*.gif
**/.github/
endef

# syntax: <URL>|<name>
# names must be unique
# URLs are expected to be .tar.gz files with an additional directory wrapping stuff
# some dependencies:
# helpful: dash s f elisp-refs
# compat is widely used
PACKAGES :=\
	https://github.com/minad/marginalia/archive/refs/tags/2.11.tar.gz|marginalia \
	https://github.com/minad/vertico/archive/refs/tags/2.9.tar.gz|vertico \
	https://github.com/minad/corfu/archive/refs/tags/2.10.tar.gz|corfu \
	https://github.com/oantolin/orderless/archive/refs/tags/1.7.tar.gz|orderless \
	https://github.com/minad/cape/archive/refs/tags/2.7.tar.gz|cape \
	https://github.com/minad/consult/archive/refs/tags/3.5.tar.gz|consult \
	https://github.com/Wilfred/helpful/archive/refs/tags/0.21.tar.gz|helpful \
	https://github.com/magnars/dash.el/archive/refs/tags/2.20.0.tar.gz|dash \
	https://github.com/magnars/s.el/archive/refs/tags/1.13.0.tar.gz|s \
	https://github.com/rejeep/f.el/archive/refs/tags/v0.21.0.tar.gz|f \
	https://github.com/Wilfred/elisp-refs/archive/refs/tags/1.5.tar.gz|elisp-refs \
	https://github.com/jdtsmith/ultra-scroll/archive/refs/tags/v0.6.2.tar.gz|ultra-scroll \
	https://github.com/protesilaos/pulsar/archive/refs/tags/1.3.4.tar.gz|pulsar \
	https://github.com/protesilaos/doric-themes/archive/refs/tags/1.1.0.tar.gz|doric-themes \
	https://github.com/bbatsov/tokyo-night-emacs/archive/refs/tags/v1.0.0.tar.gz|tokyo-night \
	https://github.com/bbatsov/guru-mode/archive/refs/tags/v1.0.tar.gz|guru-mode \
	https://github.com/emacs-compat/compat/archive/refs/tags/31.0.0.1.tar.gz|compat \
	https://github.com/Fuco1/smartparens/archive/refs/tags/1.11.0.tar.gz|smartparens

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
