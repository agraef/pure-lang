" Vim syntax file
" Language:    Pure
" Maintainers: Albert Graef <Dr.Graef@t-online.de>
" Last Change: 2008 Apr 23
" URL:	       http://pure-lang.sf.net

" This was mostly pilfered from vim's Prolog mode by Thomas Koehler.
" Only basic highlighting of comments, numbers, strings and variable
" symbols is supported right now.

" To install, copy this file to ~/.vim/syntax and add the following
" entry to the ~/.vim/filetype.vim file so that Vim recognizes the
" '.pure' file type:
" augroup filetypedetect
" au BufNewFile,BufRead *.pure	setf pure
" augroup END

" Quit when a syntax file was already loaded
if version < 600
   syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Pure is case sensitive.
syn case match

" comments and strings

syn region   pureCComment	start=+/\*+ end=+\*/+
syn match    pureComment	+//.*+
syn region   pureString	        start=+L\="+ skip=+\\\\\|\\"+ end=+"+ 


" keywords
syn keyword pureKeyword	infix infixl infixr outfix prefix postfix
syn keyword pureKeyword namespace nonfix private public
syn keyword pureKeyword	case const def else end extern if let of otherwise then
syn keyword pureKeyword	using when with
syn keyword pureSpecial catch throw
syn keyword pureType bigint bool char short int long double
syn keyword pureType expr string pointer void
syn keyword pureType int8 int16 int32 int64
syn keyword pureType matrix dmatrix cmatrix imatrix

syn match   pureNumber	         "\<[0-9]*\>"
syn match   pureHexNumber	 "\<0[Xx][0-9A-Fa-f]*\>"
" syn match   pureIdentifier       "\<[A-Za-z_][A-Za-z0-9_]*\>"
syn match   pureCommentError     "\*/"
" syn match   pureSpecialCharacter ";"
" syn match   pureSpecialCharacter "::"

" synchronization; you might have to fiddle with this
syn sync minlines=50
syn sync maxlines=500

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pure_syn_inits")
  if version < 508
    let did_pure_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink pureComment		Comment
  HiLink pureCComment		Comment
  HiLink pureCharCode		Special

  HiLink pureKeyword		Keyword
  HiLink pureSpecialCharacter	Special
  HiLink pureNumber		Number
  HiLink pureHexNumber		Number
  HiLink pureType		Type
  HiLink pureSpecial		Identifier
  HiLink pureIdentifier		Identifier
  HiLink pureCommentError	Error
  HiLink pureString		String

  delcommand HiLink
endif

let b:current_syntax = "pure"

" vim: ts=8
