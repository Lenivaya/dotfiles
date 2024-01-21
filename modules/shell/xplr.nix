{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  inherit (config.env) TERM;

  cfg = config.modules.shell.xplr;
in {
  options.modules.shell.xplr.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      xplr
      (makeDesktopItem {
        name = "xplr";
        desktopName = "XPLR";
        icon = "utilities-terminal";
        exec = "${TERM} -e xplr";
        categories = ["FileManager" "FileTools" "System"];
      })

      # images preview
      imv
      # Storage usage
      dua
      # batch rename
      pipe-rename
      # better cd
      zoxide
    ];

    home.configFile."xplr" = {
      source = "${configDir}/xplr";
      recursive = true;
    };

    environment.shellAliases = {
      xcd = ''cd "$(xplr --print-pwd-as-result)"'';
    };
  };
}
