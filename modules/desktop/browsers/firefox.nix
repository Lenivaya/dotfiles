{ config, options, lib, pkgs, ... }:
with lib; {
  options.modules.desktop.browsers.firefox = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.browsers.firefox.enable {

    my.packages = with pkgs; [
      firefox-bin
      (pkgs.writeScriptBin "firefox-private" ''
        #!${stdenv.shell}
        ${firefox}/bin/firefox --private-window "$@"
      '')
      (makeDesktopItem {
        name = "firefox-private";
        desktopName = "Firefox (Private)";
        genericName = "Open a private Firefox window";
        icon = "firefox";
        exec = "${firefox-bin}/bin/firefox --private-window";
        categories = "Network";
      })
    ];

    my.env.XDG_DESKTOP_DIR = "$HOME"; # prevent firefox creating ~/Desktop
  };
}
