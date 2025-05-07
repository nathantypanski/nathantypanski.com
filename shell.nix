{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-23.11.tar.gz") {}
  # Or use a specific commit:
  # { pkgs ? import (fetchTarball {
  #     url = "https://github.com/NixOS/nixpkgs/archive/YOUR_CHOSEN_COMMIT_HASH.tar.gz";
  #     # You'll need to get the sha256 for that tarball, e.g., using nix-prefetch-url
  #     sha256 = "YOUR_SHA256_HASH";
  #   }) {}
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
    haskell.compiler.ghc902
    sass
  ];
}
