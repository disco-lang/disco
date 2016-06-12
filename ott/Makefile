PDFLATEX = pdflatex
BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_show_meta false
SKIM := skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

all: pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath	
	@$(SKIM) simple.pdf &>/dev/null
	@$(SKIM) simple.pdf &>/dev/null
	@$(SKIM) simple.pdf &>/dev/null
  endif

pdf : simple.pdf

simple.tex : simple.ott
	$(OTT) $(OTT_FLAGS) -i simple.ott -o simple.tex

simple.pdf : simple.tex Makefile
	$(PDFLATEX) simple.tex

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
