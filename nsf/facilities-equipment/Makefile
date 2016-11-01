PDFLATEX=pdflatex
SKIM = skim_revert.sh

all : pdf

pdf : fac-eq.pdf
	$(SKIM) $(CURDIR)/fac-eq.pdf

fac-eq.pdf : fac-eq.tex Makefile 
	$(PDFLATEX) fac-eq.tex

clean : 
	rm -f *.aux *.dvi *.ps *.log *.pdf *~ *.out *.bbl *.rel *.blg
