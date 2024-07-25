{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.env) TERM;
in
{
  user.packages =
    with pkgs;
    [
      ripgrep # fast grepper
      fd # rust alternative to find
      binutils
      usbutils

      zip
      unzip
      unrar
      ouch

      dua # space usage
      duf # space usage tables

      ps_mem
      lm_sensors
      killall

      neovim
      curl
      lnav # <- log file navigator
      procs # <- a "modern" replacement for ps

      # gnome-sound-recorder
      gnome-autoar
      gnome-system-monitor
      loupe # eog
    ]
    ++ optionals config.this.isHeadful [
      libqalculate # calculator cli w/ currency conversion
      youtube-dl
      android-file-transfer
    ]
    ++ optionals config.modules.desktop.enable [
      maim
      scrot # Screenshots
      jgmenu
      # gnome-usage
      mission-center

      brightnessctl

      xorg.xkill
      xclip
      xdotool

      # Appearance
      # qt5ct
      feh
      lxappearance

      (makeDesktopItem {
        name = "scratch-calc";
        desktopName = "Calculator";
        icon = "calc";
        exec = "${TERM} -e qalc";
        categories = [ "Development" ];
      })
    ];
}
