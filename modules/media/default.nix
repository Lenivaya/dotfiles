{ config, lib, pkgs, ... }:

{
  imports = [
    ./mpv.nix
    # ./ncmpcpp.nix
    ./spotify.nix
  ];

  my.packages = with pkgs; [ youtube-dl ];
}
