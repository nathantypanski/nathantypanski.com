{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ee01de29d2f58d56b1be4ae24c24bd91c5380cea.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    zlib
    git
    gcc
    gmp
    stack
    cabal-install
    pandoc
    haskell.compiler.ghc902
    sass
  ];
}
