" Dvx vim syntax highlighting file
"
" by silverweed
" May 2015

if exists("b:current_syntax")
	finish
endif

" Main Tokens
syn match dvxPrelude /ITALIANI[.!]/
syn keyword dvxDef DEFINENDO NOMINO OVE COME
syn keyword dvxNullCall È DI
syn keyword dvxFlow SE ALLORA SENNÒ ALTRIMENTI FINCHÉ
syn keyword dvxValue VERO FALSO NIENTE _
syn match dvxSep /[ ,!.:;]/
syn match dvxComment "U.*$"
syn match dvxString /{[^}]*}/
syn match dvxNumberLiteral /'[XIVLCDM]\+'/
syn match dvxZero /'ZERO'/

" Std functions
syn keyword dvxCoreFunc DICO ANNVNCIO DIMMI ACCAPO PIV MENO PER DIVISO MAGGIORE MINORE VGVALE DIVERSO QVALCVNO TVTTI VERITIERO NON

hi link dvxPrelude       Include
hi link dvxDef           Type
hi link dvxNullCall      Operator
hi link dvxSeparator     Operator
hi link dvxFlow          Keyword
hi link dvxValue         Number
hi link dvxString        String
hi link dvxCoreFunc      Function
hi link dvxComment       Comment
hi link dvxSep           Special
hi link dvxNumberLiteral Number
hi link dvxZero          Number

let b:current_syntax = "dvx"
