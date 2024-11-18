" Disable vsvim bell sound
set vb t_vb=

" Hide eall vsvim marks in the gutter
set vsvim_hidemarks=<>[]^.'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ

" Set clipboard to unnamedplus for system clipboard integration
set clipboard=unnamed

" Prevents new lines from automatically continuing comments
set formatoptions-=c
set formatoptions-=r
set formatoptions-=o

" Disable showing list characters
set nolist

" Set horizontal scroll options
set sidescroll=1
set sidescrolloff=10

" Disable right-click mouse functionality
noremap <RightMouse> <Nop>
inoremap <RightMouse> <Nop>
vnoremap <RightMouse> <Nop>

" Enable line numbers and relative line numbers
set number
set relativenumber

" Configure tab settings
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

" Enable smart indentation
set smartindent

" Disable line wrapping
set nowrap

" Disable search highlighting, enable incremental search
set nohlsearch
set incsearch

" Enable true color support
set termguicolors

" Set scroll offset and always show sign column
set scrolloff=8
set signcolumn=yes
set isfname+=@-@

" Set update time for faster experience
set updatetime=50

" Set color column at 80 characters
set colorcolumn=80

" Configure folding by marker
set foldmethod=marker
set foldmarker=//\ #region,//\ #endregion

