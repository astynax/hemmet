TARGET := $(HOME)/.local/bin

.PHONY: install
install:
	cabal install \
		--avoid-reinstalls \
		--offline \
		--installdir=$(TARGET) \
		--install-method=copy \
		--overwrite-policy=always

.PHONY: update-tests
update-tests:
	cabal test --test-option=--accept
