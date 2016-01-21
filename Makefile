## Var Defs

SHELL := /bin/bash

HOME_ALIAS ?= HOME
LAYER_DIR  ?= layers/common
BACKUP_DIR ?= backups
SPACEMACS  ?= https://github.com/syl20bnr/spacemacs

FILTER ?= *

## Function Defs

to_layer   = $(subst $(HOME),$(HOME_ALIAS),$(1))
from_layer = $(subst /$(HOME_ALIAS),$(HOME),$(1))

## Target Deps

LAYER_DIFFS   = $(shell find $(LAYER_DIR) -type f -wholename "$(FILTER)")
LAYER_PATHS   = $(subst $(LAYER_DIR),,$(subst .diff,,$(LAYER_DIFFS)))
LAYER_TARGETS = $(call from_layer,$(LAYER_PATHS))

## Phony Rules

.SECONDARY:

.PHONY: all bootstrap packages layer clean

all: bootstrap layer

clean: $(BACKUP_DIR) nix-binary-tarball-unpack
	rm -r $^

##################
## Bootstrap Rules
##################

bootstrap: packages $(HOME)/.emacs.d

packages: /nix
	nix-env -f packages.nix -i

/nix:
	curl https://nixos.org/nix/install | sh
  # source /Users/jtmoulia/.nix-profile/etc/profile.d/nix.sh

# spacemacs
$(HOME)/.emacs.d:
	git clone $(SPACEMACS) $@

###############
## Backup Rules
###############

# Circular dep between patch, backup, and file:
#
#             patch.diff
#             /       \
#         backup <-> file

layer: $(LAYER_TARGETS)

## BACKUP

# Backup
# e.g. BACKUP/etc/hosts
$(BACKUP_DIR)/%:
	mkdir -p $(@D)
	if [ -e "/$*" ]; then cp "/$*" "$@"; else touch "$@"; fi

## LAYER

# Layer: File in HOME
# e.g. LAYER/HOME/.bashrc
$(LAYER_DIR)/$(HOME_ALIAS)/%:
	cp $(HOME)/$* $@

# Layer: File
# e.g. LAYER/etc/hosts
$(LAYER_DIR)/%:
	cp /$* $@

# Layer: Diff in HOME
# e.g. LAYER/HOME/.bashrc.diff
$(LAYER_DIR)/$(HOME_ALIAS)/%.diff: $(BACKUP_DIR)$(HOME)/%
	mkdir -p $(@D)
	diff $(HOME)/% $< > $@; if [ $$? -gt 1 ]; then exit 1; fi

# Layer: Diff
# e.g. LAYER/etc/hosts.diff
$(LAYER_DIR)/%.diff: $(BACKUP_DIR)/%
	mkdir -p $(@D)
	diff $< /$* > $@; if [ $$? -gt 1 ]; then exit 1; fi

## CONFIG

# Config: File in HOME
# e.g. /HOME/.bashrc
$(HOME)/%: $(LAYER_DIR)/$(HOME_ALIAS)/% $(BACKUP_DIR)$(HOME)/%
	mkdir -p $(@D)
	ln -sf $(abspath $<) $@

# Config: File
# e.g. /etc/hosts
/%: $(LAYER_DIR)/% $(BACKUP_DIR)/%
	# TODO: The new file should maintain permissions
	@if [ -w "$@" -a -w "$(@D)" ]; then \
    echo "Symlinking $< -> $@"; \
		mkdir -p $(@D); \
		ln -sf $(abspath $<) $@; \
	else \
		echo "Permission required to symlink $< -> $@:"; \
		sudo mkdir -p $(@D); \
		sudo ln -sf $(abspath $<) $@; \
  fi

# Config: Diff in HOME

$(HOME)/%: $(BACKUP_DIR)/% $(LAYER_DIR)/$(HOME_ALIAS)/%.diff
	patch $^ -o $@

# Config: Diff

/%: $(BACKUP_DIR)/% $(LAYER_DIR)/%.diff
	patch $^ -o $@
