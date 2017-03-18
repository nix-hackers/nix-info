{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base-compat, stdenv
      , string-conversions, text, turtle, unordered-containers
      }:
      mkDerivation {
        pname = "nix-info";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base base-compat string-conversions text turtle
          unordered-containers
        ];
        executableHaskellDepends = [
          aeson base base-compat string-conversions text turtle
          unordered-containers
        ];
        homepage = "https://github.com/nix-hackers/nix-info";
        description = "brew info clone for Nix";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
