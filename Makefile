TARGET := $(HOME)/.local/bin

.PHONY: install
install:
	cabal install \
		--installdir=$(TARGET) \
		--install-method=copy \
		--overwrite-policy=always
