vim.out : vim.hs
	ghc vimmode.hs position.hs ioutil.hs util.hs vim.hs -package hscurses -o vim

clean:
	rm -f vim.out *.hi *.o debug.log
