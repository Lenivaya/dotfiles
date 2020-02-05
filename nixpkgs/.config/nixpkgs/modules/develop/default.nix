{ config, lib, pkgs, ... }:

{
  imports = [
    ./go.nix
    ./haskell.nix
    ./python.nix
  ];

  home.packages = with pkgs; [
  # Nix
    nixfmt

  # Shell
    shellcheck

  # Rust
    rustup

  # Lisps
    clisp
    sbcl

  # C / C++
    cmake
    clang
    llvm
    rtags
    ccls
  ];
}
