SRC_DIR = src 
TESTS_DIR = tests 
LIB_DIR = lib
MOD_DIR = mod

.PHONY=	all

all:
	$(MAKE) -C $(SRC_DIR) $@
	$(MAKE) -C $(TESTS_DIR) $@

tests: all

clean:	
	$(MAKE) -C $(SRC_DIR) $@
	$(MAKE) -C $(TESTS_DIR) $@

distclean: clean
	$(MAKE) -C $(SRC_DIR) $@
	$(MAKE) -C $(TESTS_DIR) $@
	rm -rf $(LIB_DIR) $(MOD_DIR)
