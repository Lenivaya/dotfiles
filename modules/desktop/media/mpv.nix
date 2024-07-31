{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.mpv;
in
{
  options.modules.desktop.media.mpv.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.programs.mpv = enabled // {
      package =
        with pkgs;
        let
          mpv' = mpv.override {
            scripts = with mpvScripts; [
              seekTo

              mpris
              mpv-cheatsheet
              mpv-webm

              autoload
              cutter
              convert
              uosc
              thumbfast

              sponsorblock
              quality-menu
              webtorrent-mpv-hook

              # my.mpv-autosub
            ];
          };
        in
        mpv';

      bindings = {
        "RIGHT" = "seek 10";
        "LEFT" = "seek -10";
        "UP" = "seek 60";
        "DOWN" = "seek -60";

        "Shift+RIGHT" = "no-osd seek 1 exact";
        "Shift+LEFT" = "no-osd seek -1 exact";
        "Shift+UP" = "no-osd seek 5 exact";
        "Shift+DOWN" = "no-osd seek -5 exact";
        "Alt+g" = "script-message-to seek_to toggle-seeker";

        "Alt+h" = "add sub-delay +1";
        "Alt+l" = "add sub-delay -1";

        "Alt+k" = "add sub-scale +0.1";
        "Alt+j" = "add sub-scale -0.1";

        "B" = ''cycle-values background "#000000" "#ffffff"'';
      };

      profiles = {
        "protocol.http".force-window = "immediate";
        "protocol.https".profile = "protocol.http";

        "extension.gif" = {
          cache = false;
          loop-file = true;
        };
        "extension.png" = {
          profile = "extension.gif";
          video-aspect-override = 0;
        };
        "extension.jpeg".profile = "extension.png";
        "extension.jpg".profile = "extension.png";
      };

      config = {
        screenshot-directory = "~/Pictures/movies";

        audio-file-auto = "fuzzy";
        volume = 100;
        volume-max = 200;

        ytdl = true;

        # hwdec = "auto-safe";
        # vo = "gpu";
        # profile = "gpu-hq";
      };
    };

    user.packages = with pkgs; [
      ytfzf # find and watch videos on youtube
      # youtube-tui
      # ytmdl
    ];
  };
}
