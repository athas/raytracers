with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "ray";
    buildInputs = [ dotnet-sdk_3 gnumake ];
}
