set nocompatible             " iMproved
filetype off                 " for vundle

set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

" Vundled up
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'kien/ctrlp.vim'
Bundle 'mileszs/ack.vim'
" <c-w>o
Bundle 'vim-scripts/ZoomWin'
" Bright Colores
Bundle 'altercation/vim-colors-solarized'
Bundle 'Lokaltog/vim-powerline'
" python
Bundle 'hynek/vim-python-pep8-indent'
Bundle 'klen/python-mode'


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
set wildignore+=*/tmp/*,*.swp,*.pyc
" .. and with NERDTree...
let NERDTreeIgnore = ['\.pyc$\', '\.swp$\']


" Mappings
nnoremap <leader>nt :NERDTree<CR>
nnoremap <leader>a :Ack 
