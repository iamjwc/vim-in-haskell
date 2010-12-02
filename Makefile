vim.out : vim.hs
	ghc position.hs ioutil.hs util.hs vim.hs -package hscurses

