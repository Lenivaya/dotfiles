{
  config,
  options,
  lib,
  pkgs,
  home-manager,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.apps.rofi;
  inherit (config.dotfiles) configDir;
in {
  options.modules.desktop.apps.rofi.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.programs = {
      rofi = {
        enable = true;
        package = pkgs.rofi.override {
          plugins = with pkgs; [
            rofi-emoji
            rofi-calc
            rofi-file-browser

            rofi-top
          ];
        };

        theme = "main";
        cycle = true;
        extraConfig = {
          modi = "combi,drun,emoji";
          combi-modi = "drun,emoji";
          show = "combi";
          show-icons = true;
        };
      };
    };

    home.configFile."rofi" = {
      source = "${configDir}/rofi";
      recursive = true;
    };

    user.packages = let
      rofiCommand = modi: "rofi -show ${modi}";
    in
      with pkgs; [
        wmctrl # windows bringing support
        xkb-switch # Switching of keyboard layouts

        rofi-bluetooth
        rofi-systemd
        # rofi-power-menu

        (makeDesktopItem {
          name = "Rofi-calc";
          desktopName = "Rofi: Calculator";
          icon = "calc";
          exec = rofiCommand "calc";
          categories = ["Development"];
        })
        (makeDesktopItem {
          name = "Rofi-files";
          desktopName = "Rofi: Filebrowser";
          icon = "system-file-manager";
          exec = rofiCommand "file-browser-extended";
        })
        (makeDesktopItem {
          name = "Rofi-emojis";
          desktopName = "Rofi: emoji";
          icon = "face-smile";
          exec = rofiCommand "emoji";
        })
        (makeDesktopItem {
          name = "Rofi-windows";
          desktopName = "Rofi: window switcher";
          icon = "preferences-desktop-theme"; # Looks good
          exec = rofiCommand "window -window-thumbnail";
        })
        (makeDesktopItem {
          name = "Rofi-keyboard-switcher";
          desktopName = "Rofi: Keyboard switcher";
          icon = "preferences-desktop-keyboard";
          exec = "keyboard-switch";
        })
        (makeDesktopItem {
          name = "Rofi-bluetooth";
          desktopName = "Rofi: bluetooth";
          icon = "bluetooth";
          exec = "rofi-bluetooth";
        })
        (makeDesktopItem {
          name = "Rofi-systemd";
          desktopName = "Rofi: systemd";
          icon = "systemd";
          exec = "rofi-systemd";
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
          exec = "zzz -f";
        })
        (makeDesktopItem {
          name = "lock-display";
          desktopName = "Lock screen";
          icon = "system-lock-screen";
          exec = "zzz";
        })
      ];
  };
}
