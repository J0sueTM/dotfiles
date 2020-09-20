" .vimrc file by J0sueTM

if v:progname =~? "evim"
    finish
endif

" Plugins
call plug#begin('~/.vim/autoload/')

    Plug 'jremmen/vim-ripgrep'
    Plug 'https://github.com/tpope/vim-surround.git'
    Plug 'mbbill/undotree'
    Plug 'https://github.com/preservim/nerdtree.git'
    Plug 'jiangmiao/auto-pairs'
    Plug 'https://github.com/itchyny/lightline.vim' 
    Plug 'https://github.com/vbe0201/vimdiscord.git'
    
    " color shemes 
    Plug 'https://github.com/joshdick/onedark.vim.git'
    Plug 'https://github.com/morhetz/gruvbox.git'
    Plug 'https://github.com/jacoborus/tender.vim.git'
    Plug 'dracula/vim', {'as':'dracula'}
    Plug 'https://github.com/drewtempelmeyer/palenight.vim.git'
    Plug 'https://github.com/sainnhe/sonokai.git'
    Plug 'https://github.com/gosukiwi/vim-atom-dark.git'
    Plug 'https://github.com/kyoz/purify.git'
    Plug 'https://github.com/voronianski/oceanic-next-color-scheme.git'

call plug#end()

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
set number
set wrap
set smartcase

set background=dark
colo gruvbox 

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
inoremap UH <Esc> 
"
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
