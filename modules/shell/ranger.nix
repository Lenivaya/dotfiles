{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
in
{
  user.packages = with pkgs; [
    ranger

    ueberzugpp
    poppler_utils
    zoxide
  ];

  # Maybe some time
  # home.programs.pistol =
  #   enabled
  #   // {
  #     associations = [];
  #   };

  home.configFile."ranger" = {
    source = "${configDir}/ranger";
    recursive = true;
  };
}
