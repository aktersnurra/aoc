.POSIX:

install:
	cabal install --overwrite-policy=always
	ln -s $(XDG_DATA_HOME)/cabal/bin/aoc $(HOME)/.local/bin/aoc

uninstall:
	cabal clean
	rm $(HOME)/.local/bin/aoc

.PHONY: install uninstall
