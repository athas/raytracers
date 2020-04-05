with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "futspace";
    buildInputs = [ ocl-icd opencl-headers ];
}
