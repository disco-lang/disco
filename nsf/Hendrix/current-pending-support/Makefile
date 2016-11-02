PDFLATEX=pdflatex
SKIM = skim_revert.sh

all : pdf

pdf : cps.pdf
	$(SKIM) $(CURDIR)/cps.pdf

cps.pdf : cps.tex Makefile 
	$(PDFLATEX) cps.tex

clean : 
	rm -f *.aux *.dvi *.ps *.log *.pdf *~ *.out *.bbl *.rel *.blg
