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
  Plug 'https://github.com/ctrlpvim/ctrlp.vim'
  Plug 'https://github.com/mbbill/undotree'
  Plug 'https://github.com/preservim/nerdtree'
  Plug 'https://github.com/Xuyuanp/nerdtree-git-plugin'
  Plug 'https://github.com/airblade/vim-gitgutter'
  Plug 'https://github.com/jiangmiao/auto-pairs'
  Plug 'https://github.com/honza/vim-snippets'
  Plug 'https://github.com/sheerun/vim-polyglot'
  Plug 'https://github.com/vimsence/vimsence'
  Plug 'https://github.com/thaerkh/vim-workspace'
  Plug 'https://github.com/bfrg/vim-cpp-modern'
  Plug 'https://github.com/timonv/vim-cargo'
  Plug 'https://github.com/vim-scripts/AutoComplPop'
  Plug 'https://github.com/alvan/vim-closetag'
  Plug 'https://github.com/goballooning/vim-live-latex-preview'
  Plug 'https://github.com/wsdjeg/vim-assembly'
  Plug 'https://github.com/xuhdev/vim-latex-live-preview', { 'for': 'tex' }
  Plug 'https://github.com/neoclide/coc.nvim', {'branch': 'release'}
  Plug 'https://github.com/vim-airline/vim-airline'
  Plug 'https://github.com/vim-airline/vim-airline-themes'
  Plug 'https://github.com/tpope/vim-fugitive'
  Plug 'https://github.com/Lokaltog/vim-monotone'
  Plug 'https://github.com/sdiehl/vim-ormolu'
  Plug 'https://github.com/ollykel/v-vim'
  Plug 'https://github.com/ayu-theme/ayu-vim'
  Plug 'https://github.com/morhetz/gruvbox'
  Plug 'https://github.com/olivertaylor/vacme'
call plug#end()

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
set colorcolumn=100
set ttyfast
set wildmenu

set laststatus=2

" airline
let g:airline_powerline_fonts = 1

" git
let g:gitgutter_sign_added = '+'
let g:gitgutter_sign_modified = '*'
let g:gitgutter_sign_removed = '-'
let g:gitgutter_sign_removed_first_line = '-'
let g:gitgutter_sign_modified_removed = '-'

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
set background=dark
let ayucolor="dark"
colo gruvbox

" workspaces
let g:workspace_session_name = 'Session.vim'

" dvorak movement
set langmap=tj,nk,sl,jt,kn,ls

" keybindings
nnoremap <leader>w :ToggleWorkspace <CR>
nnoremap <C-a> :set rnu! <CR>
nnoremap <C-i> :set wrap! <CR>
nnoremap <C-d><C-s> :vsplit <CR>
nnoremap <C-d><C-h> :split <CR>
nnoremap <C-d><C-t> :tabN <CR>
nnoremap <C-d><C-n> :tabn <CR>

nnoremap cb :CargoBuild <CR>
nnoremap cr :CargoRun <CR>

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

let g:NERDTreeGitIndicatorMapCustom = {
  \ "Modified"  : "*",
  \ "Staged"    : "+",
  \ "Untracked" : "*",
  \ "Renamed"   : ">",
  \ "Unmerged"  : "=",
  \ "Deleted"   : "",
  \ "Dirty"     : "x",
  \ "Clean"     : "v",
  \ 'Ignored'   : 'i',
  \ "Unknown"   : "?"
  \ }

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
