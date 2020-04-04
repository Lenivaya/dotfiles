{ config, lib, pkgs, ... }:

{
  imports = [ ./apps ./term ./browsers ./fonts ./xorg ./themes.nix ];

  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  programs.dconf.enable = true;

}
