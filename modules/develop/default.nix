{ config, options, lib, pkgs, ... }:

{
  my.packages = with pkgs;
    [
      # Nix
      nixfmt
    ];
}
