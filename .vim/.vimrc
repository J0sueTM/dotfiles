" .vimrc file by J0sueTM

if v:progname =~? "evim"
    finish
endif

" Get the defaults that most users want.
source $VIMRUNTIME/defaults.vim

" closes the toolbar and the menubar
if (has("gui_running"))
    set guioptions -=T
    set guioptions -=m
endif

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

" set fonts
if has("gui_running")
    if has("gui_gtk2") || has("gui_gtk3")
        set guifont=Source\ Code\ Pro\ 10
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

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin('~/.vim/autoload/')
    Plugin 'VundleVim/Vundle.vim' 

    Plugin 'https://github.com/jremmen/vim-ripgrep'
    Plugin 'https://github.com/tpope/vim-surround'
    Plugin 'https://github.com/mbbill/undotree'
    Plugin 'https://github.com/preservim/nerdtree'
    Plugin 'https://github.com/jiangmiao/auto-pairs'
    Plugin 'https://github.com/honza/vim-snippets'
    Plugin 'https://github.com/itchyny/lightline.vim'
     
    " colorschemes 
    Plugin 'https://github.com/joshdick/onedark.vim'
    Plugin 'https://github.com/morhetz/gruvbox'
    Plugin 'https://github.com/jacoborus/tender.vim'
    Plugin 'https://github.com/drewtempelmeyer/palenight.vim'
    Plugin 'https://github.com/sainnhe/sonokai'
    Plugin 'https://github.com/gosukiwi/vim-atom-dark'
    Plugin 'https://github.com/ghifarit53/tokyonight-vim'
    Plugin 'https://github.com/sainnhe/forest-night'
    Plugin 'https://github.com/ayu-theme/ayu-vim'
    Plugin 'https://github.com/glepnir/oceanic-material'
    Plugin 'https://github.com/tomasiser/vim-code-dark'
    Plugin 'https://github.com/franbach/miramare'
    Plugin 'https://github.com/AlessandroYorba/Alduin'
    Plugin 'https://github.com/nanotech/jellybeans.vim'

    " personal
    Plugin 'https://gitlab.com/betseg/vim-dcrpc'

call vundle#end()
filetype plugin indent on

" vim-dcrpc stuff
let g:dcrpc_autostart = 1

" visuals stuff
set background=dark
colo alduin
