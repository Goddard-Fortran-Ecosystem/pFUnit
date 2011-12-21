.PHONY: all

TEXT_FILE = ReferenceManual.pdf
USE_PDF = NO 
PDFLATEX=$(shell which pdflatex)


ifneq ($(PDFLATEX),)
  USE_PDF = YES
endif

CONFIG_FILE = doxygen.conf
LATEX = latex
LATEX_DIR=$(shell ls $(LATEX))
EXECUTABLE = DoxygenManager.py

REFMAN=$(LATEX)/refman.pdf


all: $(REFMAN) pdf doxy


tests: all


$(REFMAN): pdf
   ifeq ($(USE_PDF),YES)
	@cp $(REFMAN) $(TEXT_FILE)
	@echo "$(TEXT_FILE) is available"
   else
	@echo "Unfortunately, no PDF file is created"
   endif


pdf: doxy
   ifeq ($(USE_PDF),YES)
	$(MAKE) -C $(LATEX) all
   else
	@echo "****************  WARNING ****************"
	@echo "Can't perform the latex due to no pdflatex"
   endif


doxy:
	./$(EXECUTABLE)


clean:
   ifneq ($(LATEX_DIR),)
	$(MAKE) -j -C $(LATEX) clean
   endif


distclean: clean
	$(RM) $(CONFIG_FILE) $(TEXT_FILE)
	$(RM) -rf html latex
