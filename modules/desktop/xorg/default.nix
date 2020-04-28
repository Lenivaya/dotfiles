{ config, options, lib, pkgs, ... }:

with lib; {
  imports = [ ./lockscreen.nix ./xmonad.nix ];

  options.modules.desktop.WM = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.desktop.WM.enable {
    my.home.services.picom = {
      enable = true;
      package = pkgs.unstable.picom;
      activeOpacity = "1.0";
      inactiveOpacity = "0.92";
      opacityRule = [
        "100:name *= 'i3lock'"
        "100:class_g = 'Gimp'"
        "100:class_g = 'Inkspace'"
        "100:class_g = 'krita'"
        "100:class_g = 'feh'"
        "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
        "96:_NET_WM_STATE@:32a *= '_NET_WM_STATE_STICKY'"
      ];
      backend = "glx";
      vSync = true;
      blurExclude = [ "window_type = 'dock'" "window_type = 'desktop'" ];
      fade = true;
      fadeDelta = 1;
      fadeSteps = [ "0.01" "0.012" ];
      shadow = true;
      shadowOffsets = [ (-10) (-10) ];
      shadowOpacity = "0.22";
      extraOptions = ''
        shadow-radius = 12;
        blur-kern = "7x7box";
        blur-strength = 320;
      '';
    };

    my.home.services.sxhkd = {
      enable = true;
      keybindings = {
        "super + Escape" = "pkill -USR1 -x sxhkd";

        # screencast region to mp4
        "super + Print" = "scrrec -s ~/recordings/$(date +%F-%T).mp4";
        # screencast region to gif
        "super + ctrl + Print" = "scrrec -s ~/recordings/$(date +%F-%T).gif";

        "super + KP_Left" = "st -e ranger";
        "super + shift + KP_Left" = "st -e nnn";
        "super + KP_Home" = "st -e tmux";
      };
    };
  };

}
