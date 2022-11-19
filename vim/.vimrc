set autoread "automatically load files that have been changed outside of vim
set encoding=utf-8

syntax on        "turn syntax highlighting on
set showmatch    "show matching brace
set textwidth=80 "hard wrap at 80 characters

""" Indentation
set copyindent  "when auto-indenting, use the indenting format of previous line
set smarttab    "<Tab? in front of a line uses 'shiftwidth' otherwise 'tabstop'
set autoindent  "copy indent from current line when starting a new line"
set smartindent "inserts an extra indentation in some cases
set tabstop=4     "a tab is four spaces
set softtabstop=4 "<BS> removes a 'tab' even if it's really spaces
set shiftwidth=4  "number of spaces to use for autoindenting

""" Interface
"map jk to escape when in insert mode  
inoremap <special> jk <ESC> 
set mouse=a " enable mouse support

" let me delete across lines and autoindents in insert mode
set backspace=indent,eol,start 

" let these keys move across lines in normal mode
set whichwrap=bs,<,>,[,],h,l

""" Searching
set hlsearch   "highlight all matches
set incsearch  "display matches as you type 
set ignorecase "ignore case in search patterns 
set smartcase  "override ignorecase when search pattern contains uppercase
set gdefault   "imply global for new searches

""" Appearance
set number relativenumber
set ruler "display the cursor row and column
set title "display the filename in the window title-bar
set colorcolumn=80 "display line numbers
" highlight ColorColumn guibg=black

"in terminal setting, set color of columncolor to black
highlight ColorColumn ctermbg=0

