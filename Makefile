vim.out : vim.hs
	ghc -Wall -outputdir rubbish vimmode.hs position.hs ioutil.hs util.hs vim.hs -package hscurses -o vim

clean:
	rm -f vim.out *.hi *.o debug.log
