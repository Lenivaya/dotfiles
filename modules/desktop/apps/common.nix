{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.env) TERM;
in {
  user.packages = with pkgs;
    [
      ripgrep # fast grepper
      fd # rust alternative to find
      # handlr # better xdg-utils in rust
      binutils
      usbutils

      zip
      unzip
      unrar
      ouch

      dua # space usage
      duf # space usage tables
      android-file-transfer

      ps_mem
      lm_sensors
      killall

      libqalculate # calculator cli w/ currency conversion

      neovim
      curl
      youtube-dl
      lnav # <- log file navigator
      procs # <- a "modern" replacement for ps
    ]
    ++ optionals config.modules.desktop.enable (with pkgs.gnome; [
      # gnome-sound-recorder
      gnome-autoar
      gnome-system-monitor
      eog
    ])
    ++ optionals config.modules.desktop.enable [
      maim
      scrot # Screenshots
      jgmenu
      gnome-usage

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
        categories = ["Development"];
      })
    ];
}
