{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.apps.dunst;
in
{
  options.modules.desktop.apps.dunst.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    modules.desktop.apps.rofi = enabled;

    home.services.dunst = enabled // {
      settings = {
        global = {
          follow = "keyboard";
          origin = "top-center";
          offset = "20x80";
          separator_color = "#c8b38d";
          indicate_hidden = "yes";
          transparency = 5;
          notification_limit = 8;
          separator_height = 2;
          gap_size = 5;
          padding = 15;
          horizontal_padding = 15;
          frame_width = 0;
          frame_color = "{color2}";
          sort = "yes";
          idle_threshold = 120;

          # Text
          font = "monospace 12";
          line_height = 0;
          markup = "full";
          format = "<b>%s</b>\n%b";
          alignment = "left";
          show_age_threshold = 60;
          word_wrap = "yes";
          ellipsize = "middle";
          ignore_newline = "no";
          stack_duplicates = "true";
          hide_duplicate_count = "false";
          show_indicators = "yes";

          # Icons
          icon_position = "left";
          max_icon_size = 45;
          enable_recursive_icon_lookup = "true";

          # History
          sticky_history = "yes";
          history_length = 20;

          # Misc/Advanced
          dmenu = "dmenu -p dunst -l 10";
          always_run_script = "true";
          title = "Dunst";
          class = "Dunst";
          ignore_dbusclose = "true";
          corner_radius = 0;

          # mouse
          mouse_left_click = "do_action";
          mouse_middle_click = "close_all";
          mouse_right_click = "close_current";
          mouse_scroll_up = "scroll_up";
          mouse_scroll_down = "scroll_down";

          close = "ctrl+space";
          close_all = "ctrl+shift+space";
          history = "ctrl+home";
          context = "ctrl+shift+period";
        };

        urgency_low = {
          timeout = 20;
          background = "#161418";
          foreground = "#d2d1d2";
          frame_color = "#161418";
        };
        urgency_normal = {
          timeout = 30;
          background = "#161418";
          foreground = "#d2d1d2";
          frame_color = "#161418";
        };
        urgency_critical = {
          timeout = 100;
          background = "#161418";
          foreground = "#d2d1d2";
          frame_color = "#161418";
        };

        play_sound =
          let
            notificationSound = "${configDir}/dunst/Submarine.wav";
            dunst_alert = pkgs.writeShellScriptBin "dunst_alert" ''
              blacklist=("Spotify")
              [[ ! ''${blacklist[*]} =~ $DUNST_APP_NAME ]] && ${getExe' pkgs.pipewire "pw-cat"} --volume 0.2 -p "${notificationSound}"
            '';
            alert = getExe dunst_alert;
          in
          {
            summary = "*";
            script = "${alert}";
          };
      };
    };

    systemd.user.services.dunst.path = with pkgs; [
      cached-nix-shell
    ];
  };
}
