srcs := \
	src/NixInfo/Types.hs \
	src/NixInfo.hs \
	app/Main.hs \
	Setup.hs

tangle = notangle \
	-L \
	-R$@ $< \
	-filter btdefn \
	| cpif $@
weave  = noweave \
	-n \
	-filter btdefn \
	-delay -index $< >$@ \
	| cpif $@

.SUFFIXES: .tex .pdf
# .nw.tex:  ; ${weave}
.tex.pdf: ; latexmk --shell-escape -pdf -outdir=tex $<

.PHONY: all build

all: build script/nix-info docs/nix-info.pdf

script/nix-info: nix-info.nw
	notangle -R$@ $< -filter btdefn | cpif $@

${srcs}: nix-info.nw ; ${tangle}

tex/%.tex: %.nw tex/%.bib ; ${weave}

docs/%.pdf: tex/%.pdf
	@ mkdir -p $(dir $@)
	@ mv $< $@

build: ${srcs} default.nix
	@ nix-build

default.nix: nix-info.cabal
	@ nix-shell -p cabal2nix --run "cabal2nix --shell .>$@"
	@ echo "generated $@"

nix-info.cabal: package.yaml
	@ nix-shell -p haskellPackages.hpack --run hpack
