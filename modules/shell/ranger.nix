{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
in {
  config = {
    user.packages = with pkgs; [
      ranger

      ueberzugpp
      poppler_utils
    ];

    home.configFile."ranger" = {
      source = "${configDir}/ranger";
      recursive = true;
    };
  };
}
