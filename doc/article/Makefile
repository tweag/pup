FILENAME = pup
PDF_ARXIV_DEPENDENCIES = # *.sty *.cls *.bst
PDF_OTHER_DEPENDENCIES = *.bib ../../flake.nix ../../flake.lock
MAIN_END_PAGE = ??
APPENDIX_FIRST_PAGE = $(shell echo $$(($(MAIN_END_PAGE) + 1)))

all: $(FILENAME).pdf

clean:
	rm -f *.aux *.bbl *.ptb *.toc *.out *.run.xml
	rm -f *.log *.bcf *.fdb_latexmk *.fls *.blg
	rm -f no-editing-marks

$(FILENAME).pdf $(FILENAME).bbl : $(FILENAME).tex $(PDF_ARXIV_DEPENDENCIES) $(PDF_OTHER_DEPENDENCIES)
	latexmk $(FILENAME).tex

continuous::
	ls $(FILENAME).tex *.ott $(PDF_ARXIV_DEPENDENCIES) $(PDF_OTHER_DEPENDENCIES) Makefile no-editing-marks | entr make -j

# .SECONDARY:
# the line above prevents deletion of temporary files, which can be helpful for debugging build problems
