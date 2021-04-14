{ config, options, lib, pkgs, ... }:

{
  config = {
    user.packages = with pkgs;
      [
        # Nix
        nixfmt
        nixpkgs-fmt
      ];
  };
}
