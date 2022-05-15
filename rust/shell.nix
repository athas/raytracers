with import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/820177eded91f3908cfc72dfee00e831ea3d0060.zip";
  sha256 = "1yqx5zy154f8057inwjp2ranizgilvpblqq31cy7nryrwj2apics";
}) {};
stdenv.mkDerivation {
  name = "ray";
  buildInputs = [ cargo rustc ];
}
