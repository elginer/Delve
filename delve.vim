" Vim syntax file
" Language   : Delve 
" Maintainers: John Morrice
" Last Change: 11 August 2009

syn keyword self self
syn match var /\(\a\|[+\-!@£$%^&*=><|\;?/]\)[^ .()\[\]]*/
syn match stmt /:!\|:\|do/
syn keyword foo fu
syn keyword method me
syn region sexp start=/(/ end=/)/ contains=dalt,dmatch,dspecial,sexp,exec,stmt,dint,sym,dcomment,self,var,foo,method
syn match dint /\d\+/
syn match sym /#\S[^ ()\[\]]*/
syn region dspecial start=/\[[^\]]*\]/ end=/\[\/[^\]]*\]/
syn match dcomment /\/\/.*/
syn keyword dmatch match
syn match dalt /->/

hi link dalt Keyword
hi link dmatch Keyword
hi link var Normal 
hi link self Keyword
hi link dcomment Comment
hi link stmt Keyword
hi link foo Keyword
hi link method Keyword
hi link sexp Function 
hi link dspecial Special
hi link sym String
hi link dint Number
