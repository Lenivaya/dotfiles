{ pkgs, ... }:

{
  imports = [ ./go.nix ./haskell.nix ./python.nix ./rust.nix ];

  my.packages = with pkgs; [
    #
    gnumake

    # Node.js
    nodejs

    # Nix
    nixfmt

    # Shell
    shellcheck

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
