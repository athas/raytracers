with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "ray";
    buildInputs = [ futhark ocl-icd opencl-headers ];
}
