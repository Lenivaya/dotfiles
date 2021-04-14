{ config, options, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.rofi;
in
{
  options.modules.desktop.apps.rofi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs = {
      rofi = {
        enable = true;
        package = pkgs.rofi.override {
          plugins = with pkgs; [ rofi-emoji rofi-calc rofi-file-browser ];
        };
        lines = 10;
        theme = "main";
        extraConfig = {
          modi = "drun";
          show-icons = true;
        };
      };
    };

    home.configFile."rofi" = {
      source = "${configDir}/rofi";
      recursive = true;
    };

    user.packages = with pkgs; [
      (makeDesktopItem {
        name = "Rofi-calc";
        desktopName = "Rofi: Calculator";
        icon = "calc";
        exec = "rofi -show calc";
        categories = "Development";
      })
      (makeDesktopItem {
        name = "Rofi-files";
        desktopName = "Rofi: Filebrowser";
        icon = "system-file-manager";
        exec = "rofi -show file-browser";
      })
      (makeDesktopItem {
        name = "Rofi-emojis";
        desktopName = "Rofi: emoji";
        icon = "face-smile";
        exec = "rofi -show emoji";
      })
      (makeDesktopItem {
        name = "reboot";
        desktopName = "System: Reboot";
        icon = "system-reboot";
        exec = "systemctl reboot";
      })
      (makeDesktopItem {
        name = "shutdown";
        desktopName = "System: Shut Down";
        icon = "system-shutdown";
        exec = "systemctl shutdown";
      })
      (makeDesktopItem {
        name = "sleep";
        desktopName = "System: Sleep";
        icon = "system-suspend";
        exec = "$DOTFILES_BIN/zzz -f";
      })
      (makeDesktopItem {
        name = "lock-display";
        desktopName = "Lock screen";
        icon = "system-lock-screen";
        exec = "$DOTFILES_BIN/zzz";
      })
    ];
  };

}
