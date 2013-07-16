BREW_INSTALL = brew install
BREW_ARGS    =
CELLAR       = /usr/local/Cellar

CASK_INSTALL = brew cask install
CASKROOM     = /opt/homebrew-cask/Caskroom

BACKUPS      = backups

.PHONY: dotfiles apps emacs wallpaper

# Configs

$(BACKUPS):
	mkdir $(BACKUPS)

$(HOME)/%: home/% $(BACKUPS)
	if [ -f $@ ]; then cp $@ $(BACKUPS)/$*; fi
	cp $< $@

dotfiles: $(HOME)/.gitconfig \
          $(HOME)/.vimrc \
          $(HOME)/.bashrc \
          $(HOME)/.bash_profile \
          $(HOME)/.tmux.conf


## Homebrew

/usr/local/bin/brew:
	ruby -e "$$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"


# Make wrap our brew installs
$(CELLAR)/aspell: BREW_ARGS = --with-lang=en
$(CELLAR)/%:
	$(BREW_INSTALL) $* $(BREW_ARGS)


# Homebrew cask

/usr/local/Library/Taps/phinze-cask:
	brew tap phinze/homebrew-cask

homebrew-cask: /usr/local/bin/brew
	$(BREW_INSTALL) brew-cask


# Cask apps

$(CASKROOM)/%: $(CELLAR)/brew-cask
	$(CASK_INSTALL) $*

apps: $(CASKROOM)/firefox \
      $(CASKROOM)/rdio \
      $(CASKROOM)/hip-chat

## Emacs

# Setup my prelude fork
$(HOME)/.emacs.d/init.el: PRELUDE_URL = https://github.com/jtmoulia/prelude.git
$(HOME)/.emacs.d/init.el: $(CELLAR)/emacs $(CELLAR)/ack $(CELLAR)/aspell
	export PRELUDE_URL="$(PRELUDE_URL)" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

emacs: $(HOME)/.emacs.d/init.el


## Etc

wallpaper: WALLPAPER = wallpapers/mars.jpg
wallpaper:
	defaults write com.apple.desktop Background '{default = {ImageFilePath = "$(abspath $(WALLPAPER))"; };}'
