{ config, options, lib, pkgs, ... }:

{
  user.packages = with pkgs;
    [
      # Nix
      nixfmt
    ];
}
