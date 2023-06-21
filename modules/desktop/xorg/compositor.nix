{lib, ...}:
with lib.my; {
  home.services.picom =
    enabled
    // {
      backend = "glx";
      vSync = true;

      activeOpacity = 1.0;
      inactiveOpacity = 0.92;
      opacityRules = [
        "100:name *= 'i3lock'"
        "100:class_g = 'Gimp'"
        "100:class_g = 'Inkspace'"
        "100:class_g = 'krita'"
        "100:class_g = 'feh'"
        "100:class_g = 'Thunderbird'"
        "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
        "96:_NET_WM_STATE@:32a *= '_NET_WM_STATE_STICKY'"
      ];

      fade = true;
      fadeDelta = 5;
      fadeSteps = [0.03 0.03];

      shadow = true;
      shadowOffsets = [(-3) (-3)];
      shadowOpacity = 0.6;
      shadowExclude = [
        "name *= 'picom'"
        "class_g = 'trayer'"
        "class_g ?= 'Notify-osd'"
        "class_g = 'Ulauncher'"
        "_GTK_FRAME_EXTENTS@:c"
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      ];
      # noDockShadow = false;

      settings = {
        inactive-dim = 0.3;
        shadow-radius = 8;

        wintypes = {
          normal = {
            fade = false;
            shadow = true;
          };
          tooltip = {
            fade = true;
            shadow = true;
            opacity = 0.75;
            focus = true;
            full-shadow = false;
          };
          # dock = { shadow = false; }
          dnd = {shadow = true;};
          popup_menu = {opacity = 1.0;};
          dropdown_menu = {opacity = 1.0;};
        };

        focus-exclude = [
          "class_g = 'xob'"
          "class_g = 'trayer'"
          "class_g *?= 'safeeyes'"
        ];

        use-damage = true;

        # Unredirect all windows if a full-screen opaque window is detected, to
        # maximize performance for full-screen windows. Known to cause
        # flickering when redirecting/unredirecting windows.
        unredir-if-possible = true;

        # GLX backend: Avoid using stencil buffer, useful if you don't have a
        # stencil buffer. Might cause incorrect opacity when rendering
        # transparent content (but never practically happened) and may not work
        # with blur-background. My tests show a 15% performance boost.
        # Recommended.
        glx-no-stencil = true;

        # Use X Sync fence to sync clients' draw calls, to make sure all draw
        # calls are finished before picom starts drawing. Needed on
        # nvidia-drivers with GLX backend for some users.
        xrender-sync-fence = true;

        # Other
        mark-wmwin-focused = false;
        mark-ovredir-focused = false;
        use-ewmh-active-win = true;
        detect-rounded-corners = true;
        detect-client-opacity = true;
        detect-transient = true;
        detect-client-leader = true;
      };
    };
}
