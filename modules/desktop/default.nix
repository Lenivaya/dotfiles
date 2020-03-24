{ config, lib, pkgs, ... }:

let unstable = import <unstable> { };
in {
  imports = [
    ./apps/rofi.nix # My launcher
    ./apps/zathura.nix # Reader
    ./apps/dunst.nix # Notifications

    ./lockscreen.nix
    ./fonts
    ./themes.nix
  ];

  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  programs.dconf.enable = true;

  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = lib.mkDefault false;
    autoRepeatDelay = 250;
    autoRepeatInterval = 50;
    libinput.enable = true;
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

  my.home.services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Escape" = "pkill -USR1 -x sxhkd";
      "super + KP_Left" = "st -e ranger";
      "super + shift + KP_Left" = "st -e nnn";
      "super + KP_Home" = "st -e tmux";
    };
  };

  my.home.services.compton = {
    enable = true;
    package = pkgs.unstable.picom;
    activeOpacity = "1.0";
    inactiveOpacity = "0.8";
    opacityRule = [
      "100:name *= 'i3lock'"
      "95:class_g *?= 'tabbed'"
      "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      "96:_NET_WM_STATE@:32a *= '_NET_WM_STATE_STICKY'"
    ];
    backend = "glx";
    blurExclude = [ "window_type = 'dock'" "window_type = 'desktop'" ];
    fade = true;
    fadeDelta = 5;
    fadeSteps = [ "0.03" "0.03" ];
    shadow = true;
    shadowOffsets = [ 1 1 ];
    shadowOpacity = "0.3";
    vSync = "opengl-swc";
  };

}
