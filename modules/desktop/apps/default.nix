{ pkgs, ... }:

{
  imports = [
    ./common.nix

    ./dunst.nix
    ./rofi.nix
    ./discord.nix
    ./zathura.nix
    ./sxiv.nix
  ];

}
