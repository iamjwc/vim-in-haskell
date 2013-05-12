.PHONY: vim clean

vim:
	ghc -Wall -outputdir rubbish -c lib/* --make Vim.hs -o vim

clean:
	rm vim debug.log
	rm -rf ./rubbish
