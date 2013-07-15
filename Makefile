LN_TARGET = if [ ! -f $@ ]; then ln -s $(abspath $<) $@; fi
BUNDLE = vim/bundle

PHONY: install ~/.vimrc ~/.bashrc ~/.gitignore ~/.gitconfig

vim/bundle:
	mkdir -p $@

vim/autoload:
	mkdir -p $@

vim/autoload/pathogen.vim: vim/bundle vim/autoload
	curl -Sso vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

vim/bundle/syntastic: vim/autoload/pathogen.vim
	git clone http://github.com/scrooloose/syntastic $@

vim/bundle/vim-colors-solarized: vim/autoload/pathogen.vim
	git clone http://github.com/altercation/vim-colors-solarized $@

vim/bundle/ctrlp.vim: vim/autoload/pathogen.vim
	git clone http://github.com/kien/ctrlp.vim $@

vim/bundle/vimerl: vim/autoload/pathogen.vim
	git clone http://github.com/oscarh/vimerl $@

bundle: vim/bundle/syntastic vim/bundle/vim-colors-solarized \
	vim/bundle/ctrlp.vim vim/bundle/vimerl

.vimrc: vimrc
	$(LN_TARGET)

~/.vim: bundle
	$(LN_TARGET)

~/.bashrc: bashrc
	$(LN_TARGET)

~/.gitignore: gitignore
	$(LN_TARGET)

~/.gitconfig: gitconfig
	$(LN_TARGET)

install: ~/.vimrc ~/.vim ~/.bashrc ~/.gitignore ~/.gitconfig
