.PHONY: all

USERGUIDE = UserGuide
API = API

all:
	$(MAKE) -j -C $(USERGUIDE) all
	$(MAKE) -j -C $(API) all

clean:
	$(MAKE) -C $(USERGUIDE) clean
	$(MAKE) -C $(API) clean

distclean: clean
	$(MAKE) -C $(USERGUIDE) distclean
	$(MAKE) -C $(API) distclean 

tests: all
