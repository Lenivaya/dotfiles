{ config, options, lib, pkgs, ... }:

with lib.my;
let cfg = config.modules.desktop.apps.rofi;
in {
  options.modules.desktop.apps.rofi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.home.programs = {
      rofi = {
        enable = true;
        # package = pkgs.rofi.override {
        #   plugins = [ rofi-emoji rofi-calc rofi-file-browser ];
        # };
        lines = 10;
        theme = "list";
        extraConfig = ''
          # rofi.modi: combi
          # rofi.combi-modi: drun,calc,file-browser
          rofi.modi: drun
          rofi.show-icons: true
        '';
      };
    };

    user.home.xdg.configFile."rofi" = {
      source = <config/rofi>;
      recursive = true;
    };

    user.packages = with pkgs; [
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
        exec = "${<bin/zzz>} -f";
      })
      (makeDesktopItem {
        name = "lock-display";
        desktopName = "Lock screen";
        icon = "system-lock-screen";
        exec = "${<bin/zzz>}";
      })
    ];
  };

}
