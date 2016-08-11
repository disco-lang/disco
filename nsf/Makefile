PDFLATEX = pdflatex
BIBTEX = bibtex

-include overrides.mk

TOPFILE = main.tex
MAKEDEPS  = Makefile
LATEXDEPS = description.lhs data-management.tex summary.tex bay-bio.tex
GENERATED = description.tex main.tex main.pdf

all : main.pdf

main.tex : $(LATEXDEPS) main.lhs
	rm -f main.tex
	lhs2TeX --verb main.lhs > main.tex

main.pdf : $(TOPFILE) $(MAKEDEPS) $(LATEXDEPS)
	dos2unix main.tex
	$(PDFLATEX) main.tex
	$(BIBTEX) main
	$(PDFLATEX) main.tex
	$(PDFLATEX) main.tex


clean:
	rm -f *~ *.aux *.bbl *.blg *.log main.tex *.ptb *.agdai *.agda.el *.hi $(GENERATED)
