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
  colors = config.modules.themes.colorScheme;
in
{
  options.modules.desktop.apps.dunst.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    modules.desktop.apps.rofi = enabled;

    home.services.dunst = enabled // {
      settings = {
        global = {
          # Geometry and placement
          follow = "keyboard";
          origin = "top-center";
          offset = "0x80"; # Increased top padding
          # The width of the notification window in pixels. This can be a single number to specify a constant width or two numbers for the minimum and maximum width. The notification will expand from the minimum width as neccesary.
          width = "(300, 600)"; # Fixed width for consistent appearance
          height = "(50, 400)"; # Increased height as requested

          # Appearance
          transparency = 5;
          notification_limit = 6;
          separator_height = 0; # Hide separator as requested
          separator_color = "#00000000"; # transparent
          gap_size = 8; # Slightly increased gap between notifications
          padding = 12; # Increased padding
          horizontal_padding = 14; # Increased horizontal padding
          frame_width = 1; # Thin frame for subtle definition
          frame_color = colors.brightBlue; # Using a color from the scheme
          corner_radius = 4; # Slight rounding of corners for modern look

          # Text styling
          font = "sans-serif 12"; # More readable font
          line_height = 4; # Added more spacing between lines
          markup = "full";
          format = "<b>%s</b>\n%b"; # Bold summary, normal body text
          alignment = "left";
          vertical_alignment = "center"; # Center content vertically

          # Text appearance
          show_age_threshold = 60;
          word_wrap = "yes";
          ellipsize = "middle";
          ignore_newline = "no";
          stack_duplicates = "true";
          hide_duplicate_count = "false";
          show_indicators = "yes";

          # Icons
          icon_position = "left";
          min_icon_size = 32; # Minimum icon size
          max_icon_size = 48; # Larger icons for better visibility
          enable_recursive_icon_lookup = "true";
          icon_theme = "Papirus"; # You can change this to match your system theme

          # History
          sticky_history = "yes";
          history_length = 20;

          # Misc/Advanced
          # dmenu = "dmenu -p dunst -l 10";
          # dmenu = "rofi -dmenu -i -p dunst -l 10";
          always_run_script = "true";
          title = "Dunst";
          class = "Dunst";
          ignore_dbusclose = "true";

          # mouse
          # mouse_left_click = "do_action";
          # mouse_middle_click = "close_all";
          # mouse_right_click = "close_current";
          # mouse_scroll_up = "history_prev";
          # mouse_scroll_down = "history_next";
          #
          # # Shortcuts
          # close = "ctrl+space";
          # close_all = "ctrl+shift+space";
          # history-pop = "ctrl+grave";
          # context = "ctrl+shift+period";
        };

        urgency_low = {
          timeout = 6; # Shorter timeout for low urgency
          background = colors.background;
          foreground = colors.foreground;
          frame_color = colors.blue;
          separator_color = "#00000000"; # transparent
          highlight = colors.brightBlue; # Added highlight color
        };
        urgency_normal = {
          timeout = 10; # Moderate timeout for normal urgency
          background = colors.background;
          foreground = colors.foreground;
          frame_color = colors.yellow;
          separator_color = "#00000000"; # transparent
          highlight = colors.brightYellow; # Added highlight color
        };
        urgency_critical = {
          timeout = 0; # No timeout for critical (stays until dismissed)
          background = colors.background;
          foreground = colors.brightWhite; # Increased contrast for critical
          frame_color = colors.red;
          separator_color = "#00000000"; # transparent
          highlight = colors.brightRed; # Added highlight color
          fullscreen = "show"; # Show on top of fullscreen applications
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
