{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  inherit (config.env) TERMINAL;
in {
  config = {
    user.packages = with pkgs; [
      xplr
      (makeDesktopItem
        {
          name = "xplr";
          desktopName = "XPLR";
          icon = "utilities-terminal";
          exec = "${TERMINAL} -e xplr";
          categories = ["FileManager" "FileTools" "System"];
        })

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
      xcd = "cd '$(xplr --print-pwd-as-result)'";
    };
  };
}
