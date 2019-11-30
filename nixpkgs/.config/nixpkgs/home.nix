{ config, pkgs, ... }:

{
  imports = [
    ./desktop.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  services = {
    kdeconnect.enable = true;

    redshift = {
      enable = true;
      provider = "geoclue2";
      extraOptions = [ "-m randr" ];
    };
  };

  programs = {
    alacritty.enable = true;
    broot.enable = true;
    bat.enable = true;
    fzf.enable = true;
    starship.enable = true;

    git = {
      userName = "Lenivaya";
      userEmail = "xocada@gmail.com";
    };
  };

}
