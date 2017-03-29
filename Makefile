noweb_src = nix-info.nw

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
	-delay \
	-index \
	-latex $< \
	| cpif $@

.SUFFIXES: .tex .pdf
.tex.pdf: ; latexmk --shell-escape -pdf -outdir=tex $<

.PHONY: all build

all: build script/nix-info docs/nix-info.pdf

package.yaml: ${noweb_src}
	${tangle}

script/nix-info: ${noweb_src}
	notangle -R$@ $< -filter btdefn | cpif $@

${srcs}: ${noweb_src} ; ${tangle}

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
