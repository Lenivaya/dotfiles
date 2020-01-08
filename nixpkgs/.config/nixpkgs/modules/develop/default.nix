{ config, lib, pkgs, ... }:

{
  imports = [
    ./go.nix
  ];

  home.packages = with pkgs; [
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
    unstable.ccls
  ];
}
