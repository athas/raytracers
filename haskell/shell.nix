{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
stdenv.mkDerivation {
  name = "ray";
  buildInputs = [ zlib zlib.out pkgconfig haskell.compiler.ghc883 cabal-install ];
}
