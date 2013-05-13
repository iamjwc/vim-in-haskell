.PHONY: vim clean

vim:
	ghc -Wall -outputdir rubbish --make Vim.hs -o vim-in-haskell

clean:
	rm vim debug.log
	rm -rf ./rubbish
