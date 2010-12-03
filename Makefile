vim.out : vim.hs
	ghc vimmode.hs position.hs ioutil.hs util.hs vim.hs -package hscurses

