" Vim syntax file
" Language:         Ouro
" Maintainer:       Daniel Keep <daniel.keep@gmail.com>
" Latest Revision:  9 March 2011
" Remark:           Added symbol literals.

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Pull in reStructuredText syntax for doc comments
syntax include @RST syntax/rst.vim

syn keyword ouroKeywords let import export macro __builtin__ range
syn keyword ouroLogicalKeywords true false
syn keyword ouroLiteralKeywords nil
syn match   ouroSymbolKeywords /#'\|#"\|#\$/
syn match   ouroSyntax /,/
syn match   ouroOperators /[-=+/*<>\\:.]\|!=\|\/\/\|\*\*\|<=\|>=\|<>\|::\|++\|(\.)/
syn keyword ouroOperators and or not mod rem

" Note that, due to limitations in Vim, we can only match against letters and
" numbers less than 0x100.  As much, the vast majority of the allowable
" characters are missing.  Oh well.
set iskeyword=48-57,65-90,97-122,170,181,186,192-214,216-246,_

syn match ouroIdent /[\x30-\x39\x41-\x5a\x61-\x7a\xaa\xb5\xba\xc0-\xd6\xd8-\xf6]\k*/
syn match ouroIdent /\$"\(\\.\|[^"]\)*"/
syn match ouroIdentUnterm /\$"\(\\.\|[^"]\)*$/

" 0 123 0.123 123.456 .456 0e10 123e-10 123e+10 123.456e10 .456e-10

" Note: don't leading + or - in match since those get lexed as operators.

" Unescaped regex:
"
"   ( \d[0-9_]* ( [.] ( \d[0-9_]* )? )? | [.] \d[0-9_]* )
"   ( [eE] [+-]? \d\d* )?
syn match ouroNumber '\(\d[0-9_]*\([.]\(\d[0-9_]*\)\?\)\?\|[.]\d[0-9_]*\)\([eE][+-]\?\d\d*\)\?'

syn match ouroString /"\(\\.\|[^"]\)*"/
syn match ouroStringUnterm /"\(\\.\|[^"]\)*$/

syn match ouroSymbol /'[\x30-\x39\x41-\x5a\x61-\x7a\xaa\xb5\xba\xc0-\xd6\xd8-\xf6]\k*/
syn match ouroSymbol /'"\(\\.\|[^"]\)*"/
syn match ouroSymbolUnterm /'"\(\\.\|[^"]\)*$/
syn match ouroSymbol /'\(#'\|#"\|#\$\|[-,=+/*<>\\:.]\|!=\|\/\/\|\*\*\|<=\|>=\|<>\|::\|++\|(\.)\)/

syn region ouroSubExpr start="(" end=")" fold transparent
syn region ouroListExpr start="\[" end="\]" fold transparent
syn region ouroMapExpr start="\[:" end=":\]" fold transparent
syn region ouroMacroCall start="{" end="}" fold transparent
syn region ouroOpCall start="([.]" end="[.]\?)" fold transparent

syn keyword ouroCmmntNote contained TODO FIXME HACK BUG XXX NOTE
syn match ouroLineCmmnt "|--.*$" contains=ouroCmmntNote
syn region ouroBlockCmmnt start=/(--/ end=/--)/ contains=ouroBlockCmnt,ouroCmmntNote
syn region ouroDocCmmnt start=/(++/ end=/++)/ contains=@RST

let b:current_syntax = "ouro"

hi def link ouroKeywords Keyword
hi def link ouroLogicalKeywords Boolean
hi def link ouroLiteralKeywords Constant
hi def link ouroSymbolKeywords Operator
hi def link ouroSyntax Normal
hi def link ouroOperators Operator
hi def link ouroIdent Identifier
hi def link ouroIdentUnterm Error
hi def link ouroNumber Float
hi def link ouroString String
hi def link ouroStringUnterm Error
hi def link ouroSymbol Constant
hi def link ouroSymbolUnterm Error
hi def link ouroCmmntNote Todo
hi def link ouroLineCmmnt Comment
hi def link ouroBlockCmmnt Comment
hi def link ouroDocCmmnt PreProc

