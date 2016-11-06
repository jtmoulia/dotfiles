## Var Defs

SHELL := /bin/bash

HOME_ALIAS ?= HOME
LAYER_DIR  ?= layers/common
BACKUP_DIR ?= backups
SPACEMACS  ?= https://github.com/syl20bnr/spacemacs
BASHIT     ?= https://github.com/Bash-it/bash-it

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

.PHONY: all bootstrap layer clean

all: bootstrap layer

clean: $(BACKUP_DIR)
	rm -ri $^

##################
## Bootstrap Rules
##################

bootstrap: $(HOME)/.emacs.d $(HOME)/.bash_it

# spacemacs
$(HOME)/.emacs.d:
	git clone $(SPACEMACS) $@

# Bash-it
$(HOME)/.bash_it:
	git clone --depth=1 $(BASHIT) $@
	$@/install.sh

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
	mkdir -p $(@D)
	cp $(HOME)/$* $@

# Layer: File
# e.g. LAYER/etc/hosts
$(LAYER_DIR)/%:
	mkdir -p $(@D)
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
	cp $(abspath $<) $@

# Config: File
# e.g. /etc/hosts
# TODO: The new file should maintain permissions
/%: $(LAYER_DIR)/% $(BACKUP_DIR)/%
	@if [ -w "$@" -a -w "$(@D)" ]; then \
		echo "Copying $< -> $@"; \
		mkdir -p $(@D); \
		cp $(abspath $<) $@; \
	else \
		echo "Permission required to copy $< -> $@:"; \
		sudo mkdir -p $(@D); \
		sudo cp $(abspath $<) $@; \
	fi

# Config: Diff in HOME

$(HOME)/%: $(BACKUP_DIR)/% $(LAYER_DIR)/$(HOME_ALIAS)/%.diff
	patch $^ -o $@

# Config: Diff

/%: $(BACKUP_DIR)/% $(LAYER_DIR)/%.diff
	patch $^ -o $@
