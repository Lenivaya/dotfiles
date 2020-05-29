{ config, options, lib, pkgs, ... }:

{
  imports =
    [ ./go.nix ./docker.nix ./haskell.nix ./node.nix ./python.nix ./rust.nix ];

  my.packages = with pkgs; [
    # Makefiles
    gnumake

    # Nix
    nixfmt

    # Shell
    shellcheck
    shfmt

    # Lisps
    clisp
    sbcl

    # C / C++
    gcc
    cmake
    clang
    llvm
    rtags
    ccls
  ];
}
