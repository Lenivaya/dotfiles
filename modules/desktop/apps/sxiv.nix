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
  config = mkIf config.modules.desktop.enable {
    user.packages = with pkgs; [nsxiv dmenu];
    # environment.shellAliases.sxiv = "nsxiv";

    home.configFile."nsxiv" = {
      source = "${configDir}/sxiv";
      recursive = true;
    };
  };
}
