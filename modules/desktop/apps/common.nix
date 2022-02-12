{ config, lib, pkgs, ... }: {

  user.packages = with pkgs;
    let youtube-dl = yt-dlp.override { withAlias = true; };
    in [
      # some rust apps
      ripgrep # fast grepper
      fd # rust alternative to find
      exa # ls alternative
      tokei # code statistic
      maim
      scrot # Screenshots
      dua # space usage
      ytfzf # find and watch videos on youtube
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
        exec = "${pkgs.alacritty}/bin/alacritty -e qalc";
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
      brightnessctl
    ] ++ (with pkgs.gnome; [
      gnome-sound-recorder
      adwaita-icon-theme
      sushi
      nautilus
      file-roller
      gnome-autoar
      gnome-system-monitor
      gnome-usage
      eog
      geary
    ]) ++ (with pkgs.cinnamon; [ nemo ]);
}
