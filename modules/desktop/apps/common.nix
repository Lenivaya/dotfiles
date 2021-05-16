{ config, lib, pkgs, ... }: {

  user.packages = with pkgs;
    [
      # some rust apps
      ripgrep # fast grepper
      fd # rust alternative to find
      exa # ls alternative
      tokei # code statistic
      maim
      scrot # Screenshots
      dunst # notification daemon
      dua # space usage
      ytfzf # find and watch videos on youtube
      libnotify
      binutils
      usbutils
      zip
      unrar
      ncdu # space usage
      android-file-transfer
      feh
      pandoc # Universal Markup converter
      nitrogen
      ps_mem
      lm_sensors
      # Appearance
      qt5ct
      lxappearance
      pywal
      wpgtk
      killall
      stow
      libqalculate # calculator cli w/ currency conversion
      (makeDesktopItem {
        name = "scratch-calc";
        desktopName = "Calculator";
        icon = "calc";
        exec = "st -e $SHELL -c qalc";
        categories = "Development";
      })
      neovim
      nnn
      jgmenu
      pfetch
      xorg.xkill
      xclip
      curl
      youtube-dl
      appimage-run
      tdesktop
    ] ++ (with pkgs.gnome3; [
      adwaita-icon-theme
      nautilus
      file-roller
      gnome-autoar
      eog
    ]) ++ (with pkgs.cinnamon; [ nemo ]);
}
