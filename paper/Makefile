INCLUDE_DIR = ./includes
OUTPUT_DIR = ./output
DEFAULT = deriving-via-paper
GHCI = ghci

%.ghci : %.lhs $(INCLUDE_DIR)/*
	$(GHCI) -pgmL "lhs2TeX" -optL-P$(INCLUDE_DIR): -optL--pre $<

%.ghcid : %.lhs $(INCLUDE_DIR)/*
	ghcid -c "$(GHCI) -pgmL "lhs2TeX" -optL-P$(INCLUDE_DIR): -optL--pre" $<

%.tex : %.lhs $(INCLUDE_DIR)/*
	lhs2TeX -P$(INCLUDE_DIR): --poly -o $@ $<

%.hs : %.lhs $(INCLUDE_DIR)/*
	lhs2TeX -P$(INCLUDE_DIR): --newcode -o $@ $<

%.pdf : %.tex force
	TEXINPUTS=$(INCLUDE_DIR): latexmk $(LATEXMK_FLAGS) -lualatex -output-directory=$(OUTPUT_DIR) $<
	ln -sf $(OUTPUT_DIR)/$@ .

default : $(DEFAULT).pdf

ghci : $(DEFAULT).ghci

ghcid : $(DEFAULT).ghcid

.PHONY : force default ghci ghcid
