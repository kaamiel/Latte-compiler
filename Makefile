all:
	$(MAKE) -C src all
	mv src/latc_llvm .
