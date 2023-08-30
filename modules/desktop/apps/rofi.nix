{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  inherit (config) modules;

  cfg = config.modules.desktop.apps.rofi;
in {
  options.modules.desktop.apps.rofi.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.programs.rofi =
      enabled
      // {
        plugins = with pkgs; [
          rofi-emoji
          rofi-calc
          rofi-file-browser
          rofi-top
        ];
        theme = "main";
        cycle = true;
        terminal = modules.desktop.term.default;
        extraConfig = {
          modi = comcat [
            "combi"
            "drun"
            "emoji"
            "filebrowser"
          ];
          combi-modi = comcat ["drun" "emoji"];
          show = "combi";
          show-icons = true;
        };
      };

    home.configFile."rofi" = {
      source = "${configDir}/rofi";
      recursive = true;
    };

    user.packages = let
      rofiCommand = modi: "rofi -show ${modi}";
      withTheme = theme: command: "${command} -theme ${theme}";
    in
      with pkgs;
        [
          wmctrl # windows bringing support
          xkb-switch # Switching of keyboard layouts

          rofi-bluetooth
          rofi-systemd
          rofi-pulse-select
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
            name = "Rofi-top";
            desktopName = "Rofi: top | htop";
            icon = "systemd";
            exec = withTheme "minimal" (rofiCommand "top");
          })
          # (makeDesktopItem {
          #   name = "Rofi-pass";
          #   desktopName = "Rofi: pass | password manger";
          #   icon = "password";
          #   exec = getExe rofi-pass;
          # })

          (makeDesktopItem {
            name = "Rofi-audio-output";
            desktopName = "Rofi: audio select output";
            icon = "audio";
            exec = "rofi-pulse-select sink";
          })
          (makeDesktopItem {
            name = "Rofi-audio-input";
            desktopName = "Rofi: audio select input";
            icon = "audio";
            exec = "rofi-pulse-select source";
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
        ]
        ++ optional modules.services.greenclip.enable (
          writeShellScriptBin "rofi-greenclip"
          "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"
        );
  };
}
