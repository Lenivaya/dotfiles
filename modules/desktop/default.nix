{ config, lib, pkgs, ... }:

{
  imports = [ ./apps ./term ./browsers ./fonts ./xorg ./wayland ./themes.nix ];
  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = lib.mkDefault false;
    autoRepeatDelay = 250;
    autoRepeatInterval = 50;
    libinput = {
      disableWhileTyping = true;
      enable = true;
    };
    layout = "us, ru, ua";
    xkbOptions = "grp:win_space_toggle, caps:ctrl_modifier";

    displayManager.lightdm = {
      enable = true;
      greeters.gtk.theme = {
        name = "Adwaita-dark";
        package = pkgs.gnome3.gnome_themes_standard;
      };
    };

  };
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  programs.dconf.enable = true;

}
