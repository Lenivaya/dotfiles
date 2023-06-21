{
  config,
  pkgs,
  ...
}: let
  inherit (config.env) TERMINAL;
in {
  user.packages = with pkgs; let
    youtube-dl = yt-dlp.override {withAlias = true;};
  in
    [
      ripgrep # fast grepper
      fd # rust alternative to find
      exa # ls alternative
      tokei # code statistic
      maim
      scrot # Screenshots
      dua # space usage
      duf # space usage tables
      handlr # better xdg-utils in rust
      binutils
      usbutils

      zip
      unzip
      unrar
      ouch

      ncdu # space usage
      android-file-transfer
      feh
      pandoc # Universal Markup converter
      # nitrogen
      ps_mem
      lm_sensors
      # Appearance
      # qt5ct
      lxappearance
      pywal
      wpgtk
      killall
      libqalculate # calculator cli w/ currency conversion
      (makeDesktopItem {
        name = "scratch-calc";
        desktopName = "Calculator";
        icon = "calc";
        exec = "${TERMINAL} -e qalc";
        categories = ["Development"];
      })
      neovim
      nnn
      jgmenu
      pfetch
      xorg.xkill
      xclip
      curl
      youtube-dl
      # appimage-run
      tdesktop
      brightnessctl
      gnome-usage
      # skippy-xd # window exposer
      xdotool
      lnav #     <- log file navigator
      procs #    <- a "modern" replacement for ps
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
