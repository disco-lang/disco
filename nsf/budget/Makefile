PDFLATEX=pdflatex
SKIM = skim_revert.sh

all : pdf

pdf : budget-justification.pdf
	$(SKIM) $(CURDIR)/budget-justification.pdf

intro : intro.pdf
	$(SKIM) $(CURDIR)/budget-justification.pdf

budget-justification.pdf : budget-justification.tex Makefile 
	$(PDFLATEX) budget-justification.tex

clean : 
	rm -f *.aux *.dvi *.ps *.log *.pdf *~ *.out *.bbl *.rel *.blg
