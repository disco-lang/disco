PDFLATEX = pdflatex
BIBTEX = bibtex
SKIM = skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

-include overrides.mk

TOPFILE = main.tex
MAKEDEPS  = Makefile
LATEXDEPS = description.lhs data-management.tex summary.tex bay-bio.tex
GENERATED = description.tex main.tex main.pdf

all : main.pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath	
	@$(SKIM) main.pdf &>/dev/null
	@$(SKIM) main.pdf &>/dev/null
	@$(SKIM) main.pdf &>/dev/null
  endif

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
