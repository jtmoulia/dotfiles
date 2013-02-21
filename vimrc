set nocompatible             " iMproved
filetype off                 " for vundle

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" let Vundle manage Vundle
let $GIT_SSL_NO_VERIFY = 'true'
Bundle 'gmarik/vundle'

" Vundled up
Bundle 'scrooloose/nerdcommenter'
Bundle 'kien/ctrlp.vim'
" <c-w>o
" Bright Colores
Bundle 'altercation/vim-colors-solarized'
" python
Bundle 'hynek/vim-python-pep8-indent'


filetype plugin indent on

" This is dangerous, but I hate it.
set noswapfile

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

" Trailing whitespace
set list
set listchars=trail:·,tab:\|\

" Searching ignore
set wildignore+=*/tmp/*,*.swp,*.pyc,*/var/*
" .. and with NERDTree...
let NERDTreeIgnore = ['\.pyc$\', '\.swp$\']


" Mappings
nnoremap <leader>nt :NERDTree<CR>
nnoremap <leader>a :Ack 
