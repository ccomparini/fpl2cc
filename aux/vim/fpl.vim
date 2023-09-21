" Vim syntax file
" Language: FPL
" Maintainer: Chris Comparini
" Latest Revision: 5 Dec 2021

" ideally this should be generated automatically.  I'm slapping
" something at all in here though, so that I can see the pretty
" colors.

if exists("b:current_syntax")
  finish
endif

syn keyword	fplTodo	contained TODO FIXME XXX
syn region	fplComment	start="#" skip="\\$" end="$" keepend contains=fplTodo

" @directives:
syn match	fplAtDirective	"@comment_style.*"
syn match	fplAtDirective	"@default_action "
syn match	fplAtDirective	"@default_main"
syn match	fplAtDirective	"@generate_types"
syn match	fplAtDirective	"@goal.*"
syn match	fplAtDirective	"@grammar.*"
syn match	fplAtDirective	"@import"
syn match	fplAtDirective	"@internal"
syn match	fplAtDirective	"@main"
syn match	fplAtDirective	"@post_parse"
syn match	fplAtDirective	"@post_reduce"
syn match	fplAtDirective	"@produces.*"
syn match	fplAtDirective	"@scanner"
syn match	fplAtDirective	"@separator.*"
syn match	fplAtDirective	"@type_for.*"

syn match	fplQuantifier	"[*+?]"

" operators (such as they are):
syn match	fplOperator	"->"
syn match	fplOperator	";"
syn match	fplOperator	"+{"
syn match	fplOperator	"}+"
syn match	fplOperator	":"
syn match	fplOperator	"("
syn match	fplOperator	")"
syn match	fplOperator	","
syn match	fplOperator	"\["
syn match	fplOperator	"\]"

" imports
syn match	fplImport	"`.*`"

" expressions:
syn region	fplExpression	start="/" end="/" skip=+\\/+
syn region	fplExpression	start=+"+ end=+"+ skip=+\\"+
syn region	fplExpression	start=+'+ end=+'+ skip=+\\'+
syn match	fplExpression	"&[a-zA-Z_]\+"
syn match	fplExpression	"!"
syn match	fplExpression	"\~"

" expression suffixes:
syn match	fplSuffix	"\^"

" rules (treat as variables)
syn match	fplProductionName	"[A-Za-z][A-Za-z0-9_]*"
syn match	fplProductionName	"\."
syn match	fplProductionName	"<<"
syn match	fplProductionName	">"
syn match	fplProductionName	"<<"
syn match	fplProductionName	">"

" embedded c++
syntax include @CPP syntax/cpp.vim
syntax region cppSnip matchgroup=fplSnip start="+{" end="}+" contains=@CPP
" :hi link Snip SpecialComment

hi def link fplAtDirective	PreProc
hi def link fplComment	Comment
hi def link fplExpression	Constant
hi def link fplImport	PreProc
hi def link fplOperator	Operator
hi def link fplProductionName	Identifier
hi def link fplQuantifier	Repeat
hi def link fplSnip	Operator
hi def link fplSuffix	Special
hi def link fplTodo	Todo


