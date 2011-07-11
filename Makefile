.PHONY: vim clean

vim:
	ghc -Wall -outputdir rubbish --make Vim.hs -o vim

clean:
	rm -rf vim ./rubbish debug.log
