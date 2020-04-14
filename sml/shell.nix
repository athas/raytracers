with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "ray";
    buildInputs = [ polyml gnumake ];
}
