{ pkgs, config, ... }:

{
  home-manager.users.${config.user.name}.services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    experimentalBackends = true;

    activeOpacity = "1.0";
    inactiveOpacity = "0.92";
    inactiveDim = "0.3";
    opacityRule = [
      "100:name *= 'i3lock'"
      "100:class_g = 'Gimp'"
      "100:class_g = 'Inkspace'"
      "100:class_g = 'krita'"
      "100:class_g = 'feh'"
      "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      "96:_NET_WM_STATE@:32a *= '_NET_WM_STATE_STICKY'"
    ];
    blurExclude = [ "window_type = 'dock'" "window_type = 'desktop'" ];

    fade = true;
    fadeDelta = 1;
    fadeSteps = [ "0.01" "0.012" ];

    shadow = true;
    shadowOffsets = [ (-7) (-7) ];
    shadowOpacity = "0.7";
    shadowExclude = [
      "_GTK_FRAME_EXTENTS@:c"
      "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
    ];
    noDockShadow = false;

    extraOptions = ''
      blur-kern = "7x7box";
      blur-strength = 320;

      shadow-radius = 7;

      unredir-if-possible = true;
      dbe = true;
    '';
  };

}
