{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, abacate, base, mtl, regex-posix, stdenv, text
      }:
      mkDerivation {
        pname = "cucumber";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ cabal-install abacate base mtl regex-posix text ];
        executableHaskellDepends = [ cabal-install abacate base mtl regex-posix text ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
