{ lib, pkgs, ... }:
with lib;
with lib.my;
{
  # modules.dev = {
  #   nix = enabled;
  # };

  user.packages = with pkgs; [ just ];
}
