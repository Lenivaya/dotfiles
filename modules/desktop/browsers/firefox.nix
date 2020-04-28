{ config, options, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.browsers.firefox = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    profileName = mkOption {
      type = types.str;
      default = config.my.username;
    };
  };

  config = mkIf config.modules.desktop.browsers.firefox.enable {

    my.home.programs.firefox = {
      enable = true;
      package = pkgs.firefox;
    };

    my.packages = with pkgs;
      [
        (makeDesktopItem {
          name = "firefox-private";
          desktopName = "Firefox (Private)";
          genericName = "Open a private Firefox window";
          icon = "firefox";
          exec = "${firefox}/bin/firefox --private-window";
          categories = "Network";
        })
      ];

    my.env.XDG_DESKTOP_DIR = "$HOME"; # prevent firefox creating ~/Desktop

    my.home.home.file = let cfg = config.modules.desktop.browsers.firefox;
    in {
      ".mozilla/firefox/profiles.ini".text = ''
        [Profile0]
        Name=default
        IsRelative=1
        Path=${cfg.profileName}.default
        Default=1
        [General]
        StartWithLastProfile=1
        Version=2
      '';
    };

    my.home.programs.firefox.profiles.default.settings = {
      "browser.tabs.closeWindowWithLastTab" = false;

      # Hardware-acceleration
      "layers.acceleration.force-enabled" = true;
      "layers.omtp.enabled" = true;
      "layout.display-list.retain" = true;
      "layout.display-list.retain.chrome" = true;
    };

  };
}
