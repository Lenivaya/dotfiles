{ config, options, lib, pkgs, ... }:

{
  imports = [ ./go.nix ./haskell.nix ./node.nix ./python.nix ./rust.nix ];

  my.packages = with pkgs; [
    #
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
    cmake
    clang
    llvm
    rtags
    ccls
  ];
}
