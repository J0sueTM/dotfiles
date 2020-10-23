" .vimrc file by J0sueTM

if v:progname =~? "evim"
    finish
endif

" Get the defaults that most users want.
source $VIMRUNTIME/defaults.vim

set nocompatible
" Plugins
call plug#begin('~/.vim/plugged/')
    Plug 'https://github.com/jremmen/vim-ripgrep'
    Plug 'https://github.com/tpope/vim-surround'
    Plug 'https://github.com/mbbill/undotree'
    Plug 'https://github.com/preservim/nerdtree'
    Plug 'https://github.com/jiangmiao/auto-pairs'
    Plug 'https://github.com/honza/vim-snippets'
    Plug 'https://github.com/itchyny/lightline.vim'
    Plug 'https://github.com/sheerun/vim-polyglot'
     
    " colorschemes
    Plug 'https://github.com/joshdick/onedark.vim'
    Plug 'https://github.com/morhetz/gruvbox'
    Plug 'https://github.com/jacoborus/tender.vim'
    Plug 'https://github.com/drewtempelmeyer/palenight.vim'
    Plug 'https://github.com/sainnhe/sonokai'
    Plug 'https://github.com/gosukiwi/vim-atom-dark'
    Plug 'https://github.com/ghifarit53/tokyonight-vim'
    Plug 'https://github.com/sainnhe/forest-night'
    Plug 'https://github.com/ayu-theme/ayu-vim'
    Plug 'https://github.com/glepnir/oceanic-material'
    Plug 'https://github.com/tomasiser/vim-code-dark'
    Plug 'https://github.com/franbach/miramare'
    Plug 'https://github.com/AlessandroYorba/Alduin'
    Plug 'https://github.com/nanotech/jellybeans.vim'
    Plug 'https://github.com/phanviet/vim-monokai-pro'

    " personal
    Plug 'https://github.com/vbe0201/vimdiscord' 

    call plug#end()

" dvorak movement
set langmap=tj,nk,sl,jt,kn,ls

" no error sounds
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" visual
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set smartcase
set cursorline
set nu
set nowrap
set background=dark
colo alduin

" keybindings
nnoremap <C-a> :set rnu! <CR>
nnoremap <C-i> :set wrap! <CR>
nnoremap <C-d><C-s> :vsplit <CR>
nnoremap <C-d><C-h> :split <CR>
nnoremap <C-d><C-t> :tabN <CR>
nnoremap <C-d><C-n> :tabn <CR>

" lightline stuff
set laststatus=2
let g:lightline = {
   \ 'colorscheme': 'jellybeans'
   \ }

" set fonts and theme
if has("gui_running")
    set guioptions -=T
    set guioptions -=m

    if has("gui_gtk2") || has("gui_gtk3")
        set guifont=Liberation\ Mono\ 10
        colo miramare
    elseif has("gui_win32")
        set guifont=Consolas:h10:cANSI
    endif
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
inoremap HL <Esc> 

" NERDTree stuff
map <C-o> :NERDTreeToggle<CR>
let g:NERDTreeWinPos="right"

" live refresh
set autoread

" compile
map <F5> :call CompileRun()<CR>
func! CompileRun()
exec "w"
if &filetype == 'c'
exec "!gcc % -o %< && ./%<"
elseif &filetype == 'cpp'
exec "!g++ % -o %< && ./%<"
elseif &filetype == 'java'
exec "!javac % && java -cp %:p:h %:t:r"
elseif &filetype == 'sh'
exec "!./%"
elseif &filetype == 'python'
exec "!python3 %"
elseif &filetype == 'html'
exec "!firefox % &"
elseif &filetype == 'md'
exec "!~/.vim/markdown.pl % > %.html &"
exec "!firefox %.html &"
endif
endfunc

" folding
set foldmethod=indent
set foldlevel=99
nnoremap <space> za

if &t_Co > 2 || has("gui_running")
	set hlsearch
endif

augroup vimrcEx
au!

autocmd FileType text setlocal textwidth=78
au FileType cpp setl ofu=ccomplete#CompleteCpp
augroup END

if has('syntax') && has('eval')
	packadd! matchit
endif
