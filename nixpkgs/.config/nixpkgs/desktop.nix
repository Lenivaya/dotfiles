{ pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    vim neovim tmux
    maim scrot
    sxhkd
    qt5ct lxappearance
    neofetch
    gnome3.nautilus gnome3.file-roller nnn # files
    usbutils unzip zip unrar dolphin 
    ncdu # Disk space usage anlyzer
    ripgrep # fast grepper
    android-file-transfer
    openvpn protonvpn-cli 
    sxiv
    zathura pandoc
    adwaita-qt
    spotify
    betterlockscreen
  ];
}
