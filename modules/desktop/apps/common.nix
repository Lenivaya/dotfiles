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
      exa # ls alternative
      # tokei # code statistic
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
      tdesktop
      brightnessctl
      gnome-usage
      xdotool
      lnav # <- log file navigator
      procs # <- a "modern" replacement for ps
      comma # quickly run soft without install using nix
    ]
    ++ (with pkgs.gnome; [
      gnome-sound-recorder
      adwaita-icon-theme
      sushi
      nautilus
      file-roller
      gnome-autoar
      gnome-system-monitor
      eog
      geary
    ]);
}
