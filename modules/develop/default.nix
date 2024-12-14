{ lib, pkgs, ... }:
with lib;
with lib.my;
{
  # modules.dev = {
  #   nix = enabled;
  # };

  user.packages = with pkgs; [ just ];

  # Is needed too often to not include it in default environment
  env.LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
}
