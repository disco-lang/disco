PDFLATEX=pdflatex
SKIM = skim_revert.sh

all : pdf

pdf : work-statement.pdf
	$(SKIM) $(CURDIR)/work-statement.pdf

intro : intro.pdf
	$(SKIM) $(CURDIR)/work-statement.pdf

work-statement.pdf : work-statement.tex Makefile 
	$(PDFLATEX) work-statement.tex

clean : 
	rm -f *.aux *.dvi *.ps *.log *.pdf *~ *.out *.bbl *.rel *.blg
