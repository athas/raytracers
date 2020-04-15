{ nixpkgs ? import <nixpkgs> {} }:
with nixpkgs;
stdenv.mkDerivation {
  name = "ray";
  buildInputs = [ sbt gnumake ];
}
