" let g:syntastic_haskell_checkers=['ghc_mod']
let g:syntastic_haskell_checkers=[]

autocmd BufWritePost *.hs :GhcModCheckAsync

set path+=src