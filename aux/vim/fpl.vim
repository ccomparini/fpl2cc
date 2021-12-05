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
syn match	fplAtDirective	"@default_action"
syn match	fplAtDirective	"@default_main"
syn match	fplAtDirective	"@post_parse"
syn match	fplAtDirective	"@produces"

" operators (such as they are):
syn match	fplOperator	"->"
syn match	fplOperator	"\s;"
syn match	fplOperator	"[*+?]"
syn match	fplOperator	"+{"
syn match	fplOperator	"}+"
" imports.  this is sorta wrong.  FIXME
syn match	fplOperator	"`.*`:" 

" expressions:
syn region	fplExpression	start="/" end="/" skip=+\\/+
syn region	fplExpression	start=+"+ end=+"+
syn region	fplExpression	start=+'+ end=+'+

" rules (treat as variables)
syn match	fplProductionName	"[A-Za-z][A-Za-z0-9_]*"

" embedded c++
syntax include @CPP syntax/cpp.vim
syntax region cppSnip matchgroup=fplSnip start="+{" end="}+" contains=@CPP
" :hi link Snip SpecialComment

hi def link fplComment	Comment
hi def link fplTodo	Todo
hi def link fplAtDirective	PreProc
hi def link fplOperator	Operator
hi def link fplExpression	Constant
hi def link fplProductionName	Identifier
hi def link fplSnip	Operator


