{ config, lib, pkgs, ... }: {
  my.packages = with pkgs;
    [
      # some rust apps
      ripgrep # fast grepper
      fd # rust alternative to find
      exa # ls alternative
      tokei # code statistic
      maim
      scrot # Screenshots
      dunst # notification daemon
      libnotify
      usbutils
      unzip
      zip
      unrar
      ncdu # Disk space usage anlyzer
      android-file-transfer
      feh
      pandoc # Universal Markup converter
      unoconv # for converting odt to pdf
      nitrogen
      ps_mem
      pulsemixer
      pamix
      lm_sensors
      vscode
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
        exec = "st -e $SHELL -c 'qalc'";
        categories = "Development";
      })
      neovim
      nnn
      jgmenu
      tdesktop
      pfetch
    ] ++ (with pkgs.gnome3; [
      adwaita-icon-theme
      nautilus
      file-roller
      gnome-autoar
      eog
    ]) ++ (with pkgs.cinnamon; [ nemo ]);
}
