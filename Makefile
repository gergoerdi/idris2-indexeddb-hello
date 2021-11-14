IDRIS2	:= $(HOME)/.idris2/bin/idris2

.PHONY: all
all: 
	$(IDRIS2) --build hello-idb.ipkg
