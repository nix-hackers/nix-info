.PHONY: all build

all: build

build: default.nix
	@ nix-build

nix-info.cabal: package.yaml
	@ nix-shell -p haskellPackages.hpack --run hpack

default.nix: nix-info.cabal
	@ nix-shell -p cabal2nix --run "cabal2nix --shell .>$@"
	@ echo "generated $@"
