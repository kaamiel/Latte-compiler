all:
	$(MAKE) -C lib all
	$(MAKE) -C src all
	mv src/latc_llvm .
