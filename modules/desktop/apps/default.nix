{ pkgs, ... }:

let unstable = import <unstable> { };
in {
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
      sxiv 
      pandoc 
      nitrogen 
      ps_mem 
      pulsemixer 
      pass 
      lm_sensors 
      vscode 
      discord 
      spotify 
      # Appearance 
      qt5ct 
      lxappearance 
      plasma5.breeze-qt5 
      pywal 
      wpgtk 
      killall 
      stow 
    ] ++ (with pkgs.unstable; [ 
      vim 
      neovim 
      nnn 
      ranger 
      jgmenu 
      tdesktop 
      pfetch 
    ]) ++ (with pkgs.gnome3; [ 
      adwaita-icon-theme 
      nautilus 
      file-roller 
      gnome-autoar 
      eog 
    ]) ++ (with pkgs.pantheon; [ elementary-camera elementary-calculator ]); 
}

