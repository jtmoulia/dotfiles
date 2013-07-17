BREW_INSTALL = brew install
BREW_ARGS    =
CELLAR       = /usr/local/Cellar

CASK_INSTALL = brew cask install
CASKROOM     = /opt/homebrew-cask/Caskroom

PIP_LIB      = /usr/local/lib/python2.7/site-packages

BACKUPS      = backups

POSTGRES_VSN = 9.2.4

.PHONY: dotfiles apps vagrant python postgres emacswallpaper


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

$(CELLAR)/brew-cask: /usr/local/bin/brew
	$(BREW_INSTALL) brew-cask


# Cask apps

$(CASKROOM)/%: $(CELLAR)/brew-cask
	$(CASK_INSTALL) $*

apps: $(CASKROOM)/firefox \
      $(CASKROOM)/rdio \
      $(CASKROOM)/hip-chat

# Note this requires user input (passwd)
vagrant: $(CASKROOM)/virutalbox \
         $(CASKROOM)/vagrant


## Emacs

# Setup my prelude fork
$(HOME)/.emacs.d/init.el: PRELUDE_URL = https://github.com/jtmoulia/prelude.git
$(HOME)/.emacs.d/init.el: $(CELLAR)/emacs $(CELLAR)/ack $(CELLAR)/aspell
	export PRELUDE_URL="$(PRELUDE_URL)" && curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh | sh

emacs: $(HOME)/.emacs.d/init.el


## Python

python: $(PIP_LIB)/pyflakes \
        $(PIP_LIB)/ipython

# Make wrap our pip installs
$(PIP_LIB)/%: $(CELLAR)/python
	pip install $*


## Erlang

erlang: $(CELLAR)/erlang


## Postgres

$(HOME)/Library/LaunchAgents:
	mkdir -p $@

$(HOME)/Library/LaunchAgents/homebrew.mxcl.postgresql.plist: \
    /usr/local/Cellar/postgresql/$(POSTGRES_VSN)/homebrew.mxcl.postgresql.plist \
    $(HOME)/Library/LaunchAgents
	cp $< $@
	sudo chmod 644 $@

postgres: python \
          $(CELLAR)/postgresql \
          $(PIP_LIB)/numpy \
          $(PIP_LIB)/psycopg2 \
          $(CELLAR)/postgis \
          $(HOME)/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
	initdb /usr/local/var/postgres -E utf8
	sudo launchctl load -w $(HOME)/Library/LaunchAgents/homebrew.mxcl.postgresql.plist



## Etc

wallpaper: WALLPAPER = wallpapers/mars.jpg
wallpaper:
	defaults write com.apple.desktop Background '{default = {ImageFilePath = "$(abspath $(WALLPAPER))"; };}'
