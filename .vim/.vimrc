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
    Plug 'https://github.com/junegunn/fzf'
    Plug 'https://github.com/tpope/vim-surround'
    Plug 'https://github.com/mbbill/undotree'
    Plug 'https://github.com/preservim/nerdtree'
    Plug 'https://github.com/ipod825/vim-netranger'
    Plug 'https://github.com/jiangmiao/auto-pairs'
    Plug 'https://github.com/honza/vim-snippets'
    Plug 'https://github.com/itchyny/lightline.vim'
    Plug 'https://github.com/sheerun/vim-polyglot'
    Plug 'https://github.com/thaerkh/vim-workspace'
    Plug 'https://github.com/bfrg/vim-cpp-modern'
    Plug 'https://github.com/joeytwiddle/sexy_scroller.vim'
    Plug 'https://github.com/vim-scripts/AutoComplPop'
    Plug 'https://github.com/alvan/vim-closetag'
    Plug 'https://github.com/preservim/tagbar'
    Plug 'https://github.com/miyakogi/sidepanel.vim'
    Plug 'https://github.com/roxma/nvim-yarp'
    Plug 'https://github.com/roxma/vim-hug-neovim-rpc'
    Plug 'https://github.com/vim-syntastic/syntastic'

    " colourschemes
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
    Plug 'https://github.com/ap/vim-css-color'
    Plug 'https://github.com/cdaddr/gentooish.vim'
    Plug 'https://github.com/dracula/vim', {'as': 'dracula'}
    Plug 'https://github.com/evprkr/galaxian-vim'
call plug#end()

" no error sounds
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" lightline stuff
set laststatus=2

" visual
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set smartindent
set smartcase
set cursorline
set nu
set nowrap
set colorcolumn=80

set termguicolors
set background=dark
colo codedark

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

" Sexy_scroller stuff
let g:SexyScroller_ScrollTime=40
let g:SexyScroller_CursorTime=70
let g:SexyScroller_MaxTime=200
let g:SexyScroller_EasingStyle=3

" set fonts and theme
if has("gui_running")
    set guioptions=M
    set guioptions-=T
    set guioptions=L
    set nomousehide           " fixed bugs on x11
    set lines=999 columns=999 " always startup maximized

    if has("gui_gtk2") || has("gui_gtk3")
        set guifont=Source\ Code\ Pro\ Semi-Bold\ 10
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

map <C-o> :SidePanel<CR>

" live refresh
set autoread

let g:sidepanel_pos = "right"
let g:sidepanel_width = 26

" Activate plugins in SidePanel
let g:sidepanel_config = {}
let g:sidepanel_config['nerdtree'] = {}
let g:sidepanel_config['tagbar'] = {}
let g:sidepanel_config['gundo'] = {}
let g:sidepanel_config['buffergator'] = {}
let g:sidepanel_config['vimfiler'] = {}
let g:sidepanel_config['defx'] = {}

let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }
let g:syntastic_c_checkers = ['gcc']
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_c_include_dirs = [getcwd() . '/inc/']
nnoremap <C-w>E :SyntasticCheck<CR>

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
    elseif &filetype == 'assembly'
        exec "!nasm -f elf64 % -o %<.o && ld %<.o -o %<"
    elseif &filetype == 'sh'
        exec "!./%"
    elseif &filetype == 'python'
        exec "!python3 %"
    elseif &filetype == 'lua'
        exec "!lua %"
    elseif &filetype == 'html'
        exec "!firefox % &"
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

" transparency
hi Normal ctermbg=none
