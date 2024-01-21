{
  config,
  pkgs,
  ...
}: let
  inherit (config.env) TERM;
in {
  user.packages = with pkgs;
    [
      ripgrep # fast grepper
      fd # rust alternative to find
      maim
      scrot # Screenshots
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
      xorg.xkill

      # Appearance
      # qt5ct
      feh
      lxappearance

      libqalculate # calculator cli w/ currency conversion
      (makeDesktopItem {
        name = "scratch-calc";
        desktopName = "Calculator";
        icon = "calc";
        exec = "${TERM} -e qalc";
        categories = ["Development"];
      })

      neovim
      jgmenu
      xclip
      curl
      youtube-dl
      telegram-desktop
      brightnessctl
      gnome-usage
      xdotool
      lnav # <- log file navigator
      procs # <- a "modern" replacement for ps
    ]
    ++ (with pkgs.gnome; [
      # gnome-sound-recorder
      gnome-autoar
      gnome-system-monitor
      eog
    ]);
}
