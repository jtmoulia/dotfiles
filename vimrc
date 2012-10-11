set nocompatible             " iMproved
filetype off                 " for vundle

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

" Vundled up
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdtree'
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
" Bright Colores
Bundle 'altercation/vim-colors-solarized'


filetype plugin indent on

set t_Co=256
syntax enable
colorscheme solarized

set number
set laststatus=2
set ruler
set incsearch
set showmatch

set autoindent
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
set nowrap
