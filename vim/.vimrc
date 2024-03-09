" .vimrc file by J0sueTM

if v:progname =~? "evim"
    finish
endif

" Get the defaults that most users want.
source $VIMRUNTIME/defaults.vim

" Plugins
set nocompatible
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin('~/.vim/plugged/')
  Plug 'ctrlpvim/ctrlp.vim'
  Plug 'mbbill/undotree'
  Plug 'jiangmiao/auto-pairs'
  Plug 'sheerun/vim-polyglot'
  Plug 'vim-scripts/AutoComplPop'
  Plug 'alvan/vim-closetag'
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'tpope/vim-fugitive'
  Plug 'Lokaltog/vim-monotone'
call plug#end()

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" no error sounds
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" visual
let g:python_recommended_style = 0
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent
set smartcase
" set cursorline
set nu
set nowrap
" set colorcolumn=100
set ttyfast
set wildmenu

set laststatus=2

" fixes error where my cursor hides near curly brackets
function! g:FuckMatchParen ()
  if exists(":NoMatchParen")
    :NoMatchParen
  endif
endfunction
augroup plugin_initialize
  autocmd!
  autocmd VimEnter * call FuckMatchParen()
augroup END

if has("termguicolors")
    set termguicolors
endif

colo monotone

" workspaces
let g:workspace_session_name = 'Session.vim'

" dvorak movement
set langmap=tj,nk,sl,jt,kn,ls

" keybindings
nnoremap <leader>w :ToggleWorkspace <CR>
nnoremap <C-a> :set rnu! <CR>
nnoremap <C-i> :set wrap! <CR>
nnoremap gl :vsplit <CR>
nnoremap lg :split <CR>
nnoremap rc :tabN <CR>
nnoremap cr :tabn <CR>

" coc
set signcolumn=yes

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gt <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" set fonts and theme
if has("gui_running")
  set go-=m
  set go-=T
  set go-=r
  set go-=L
  set go=c
  set nomousehide           " fixed bugs on x11
  set lines=999 columns=999 " always startup maximized
	set hlsearch

  set guifont=JetBrains\ Mono\ 9
endif

" organize vim files
set nobackup
set directory^=$HOME/.vim/tmp//

let &directory = expand('~/.vimdata/swap//')

set backup
let &backupdir = expand('~/.vimdata/backup//')

set undofile
let &undodir = expand('~/.vimdata/undo//')

if !isdirectory(&undodir) | call mkdir(&undodir, "p") | endif
if !isdirectory(&backupdir) | call mkdir(&backupdir, "p") | endif
if !isdirectory(&directory) | call mkdir(&directory, "p") | endif

" toggle edit mode
inoremap mw <Esc> 

" live refresh
set autoread

" folding
set foldmethod=indent
set foldlevel=99
nnoremap <space> za

augroup vimrcEx
au!

autocmd FileType text setlocal textwidth=78
au FileType cpp setl ofu=ccomplete#CompleteCpp
augroup END

if has('syntax') && has('eval')
	packadd! matchit
endif
