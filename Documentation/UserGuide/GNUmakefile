.PHONY: all

TEXT_FILE = UserGuide

USE_PDF = NO
PDFLATEX=$(shell which pdflatex)

ifneq ($(PDFLATEX),)
  USE_PDF = YES
endif

all: pdf

tests: all

pdf: $(TEXT_FILE).pdf

$(TEXT_FILE).pdf: $(TEXT_FILE).tex
   ifeq ($(USE_PDF),YES)
	( \
	pdflatex $<; \
	makeindex $(<:.tex=.idx); \
	bibtex $(<:.tex=.aux); \
	pdflatex $<; \
	while grep -q "Rerun to get cross-references right." $(<:.tex=.log); \
	do \
		pdflatex $<; \
	done \
	)
   else
	@echo "****************  WARNING ****************"   
	@echo "$(TEXT_FILE).pdf is not created due to no pdflatex in the path"
	@echo "******************************************"
   endif

clean:
	rm -f *.ps *.dvi *.aux *.toc *.idx *.ind *.ilg *.log *.blg *.bbl *.out

distclean: clean
	rm -f $(TEXT_FILE).pdf
