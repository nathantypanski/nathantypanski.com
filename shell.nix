{ pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-24.05.tar.gz";
    # Replace with the actual sha256 hash you got from nix-prefetch-url
    sha256 = "0zydsqiaz8qi4zd63zsb2gij2p614cgkcaisnk11wjy3nmiq0x1s";
  }) {}
}:


pkgs.mkShell {
  buildInputs = with pkgs; [
    pkg-config
    zlib
    git
    gcc
    gmp
    stack
    cabal-install
    pandoc
    haskell.compiler.ghc965
    sass
  ];
}
