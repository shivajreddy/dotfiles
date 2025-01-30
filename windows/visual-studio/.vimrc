" Disable bell
set vb t_vb=

" Set clipboard to unnamedplus for system clipboard integration
set clipboard=unnamed

" Remove all markers from gutter
set vsvim_hidemarks=<>[]^.'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ

" Prevents new lines from automatically continuing comments
set formatoptions-=c
set formatoptions-=r
set formatoptions-=o

" Disable showing list characters
set nolist

" Set horizontal scroll options
set sidescroll=1
set sidescrolloff=10

" Enable line numbers and relative line numbers
set number
set relativenumber

" Enable smart indentation
set smartindent

" Disable line wrapping
set nowrap

" Disable search highlighting, enable incremental search
" set nohlsearch
" set incsearch

" Disable keymaps
nnoremap <C-f> <Nop>
inoremap <C-f> <Nop>
vnoremap <C-f> <Nop>

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


"""""""""""" Keymaps from nvim """""""""""""
let mapleader = " "


" Save file
inoremap <C-s> <Esc>:w<CR>a
xnoremap <C-s> <Esc>:w<CR>
nnoremap <C-s> :w<CR>
snoremap <C-s> <Esc>:w<CR>


" Split windows
nnoremap <Leader>w' :split<CR>
nnoremap <Leader>w" :vsplit<CR>


" Yank entire buffer
function! YankEntireBuffer()
  let l:cursor_pos = getpos(".")
  execute "normal! ggVGy"
  call setpos('.', l:cursor_pos)
endfunction
nnoremap <Leader>ay :call YankEntireBuffer()<CR>

" Delete entire buffer
nnoremap <Leader>ad :%d<CR>

" Select entire buffer
nnoremap <Leader>aa ggVG


" Don't change cursor position when using J
nnoremap J mzJ`z

" Half-page jumping with centering screen
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" Searching with n, N and centering screen
nnoremap n nzzzv
nnoremap N Nzzzv

" Preserve copy buffer when pasting over highlighted text
xnoremap <leader>p "_dP

" Delete to void register
nnoremap <leader>d "_d
vnoremap <leader>d "_d

" Yank to clipboard
vnoremap <leader>y "+y
nnoremap <leader>Y "+Y

" Disable Q for recording
nnoremap Q <nop>

