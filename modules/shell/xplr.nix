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
      xplr
      # images preview
      imv
      # Storage usage
      dua
      # batch rename
      pipe-rename
    ];

    home.configFile."xplr" = {
      source = "${configDir}/xplr";
      recursive = true;
    };

    environment.shellAliases = {
      xcd = "'cd '$(xplr --print-pwd-as-result)'";
    };
  };
}
