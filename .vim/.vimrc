" .vimrc file by J0sueTM

if v:progname =~? "evim"
    finish
endif

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim 
call vundle#begin('~/.vim/autoload/')
     Plugin 'VundleVim/Vundle.vim' 

     Plugin 'jremmen/vim-ripgrep'
     Plugin 'https://github.com/tpope/vim-surround.git'
     Plugin 'mbbill/undotree'
     Plugin 'https://github.com/preservim/nerdtree.git'
     Plugin 'jiangmiao/auto-pairs'
     Plugin 'https://github.com/itchyny/lightline.vim' 
     Plugin 'https://github.com/vbe0201/vimdiscord.git'
     
     " color shemes 
     Plugin 'https://github.com/joshdick/onedark.vim.git'
     Plugin 'https://github.com/morhetz/gruvbox.git'
     Plugin 'https://github.com/jacoborus/tender.vim.git'
     Plugin 'https://github.com/drewtempelmeyer/palenight.vim.git'
     Plugin 'https://github.com/sainnhe/sonokai.git'
     Plugin 'https://github.com/gosukiwi/vim-atom-dark.git'

call vundle#end()
filetype plugin indent on

" Get the defaults that most users want.
source $VIMRUNTIME/defaults.vim

" closes the toolbar and the menubar
if (has("gui_running"))
    set guioptions -=T
    set guioptions -=m
endif

" dvorak movement
set langmap=tj,nk,sl

" no error sounds
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" visual
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set wrap
set smartcase
set nu rnu
set background=dark
colo sonokai 

set cursorline

" set fonts
if has("gui_running")
    if has("gui_gtk2") || has("gui_gtk3")
        set guifont=Source\ Code\ Pro\ Semi-Bold\ 10

        set guifont=Menlo\ Regular:h14
    elseif has("gui_win32")
        set guifont=Consolas:h11:cANSI
    endif
endif

set laststatus=2

" organe vim files
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

" file navigation
map <C-o> :NERDTreeToggle<CR>
let g:NERDTreeWinPos="right"

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

set nobackup

if &t_Co > 2 || has("gui_running")
  set hlsearch
endif

augroup vimrcEx
  au!

autocmd FileType text setlocal textwidth=78
augroup END

if has('syntax') && has('eval')
  packadd! matchit
endif
