# modules/desktop/media/recording.nix
#
# OBS to capture footage/stream, audacity for audio, handbrake to encode it all.
# This, paired with DaVinci Resolve for video editing (on my Windows system) and
# I have what I need for youtube videos and streaming.
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.recording;
in
{
  options.modules.desktop.media.recording = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt false;
    video.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.programs.obs-studio = mkIf cfg.video.enable enabled // {
      package = pkgs.obs-studio;
      plugins = with pkgs.obs-studio-plugins; [
        obs-backgroundremoval
        obs-vaapi
        obs-pipewire-audio-capture
        droidcam-obs
        obs-multi-rtmp
        looking-glass-obs
      ];
    };

    user.packages =
      with pkgs;
      # for recording and remastering audio
      (
        if cfg.audio.enable then
          with pkgs;
          [
            audacity
            ardour
          ]
        else
          [ ]
      )
      ++
        # for longer term streaming/recording the screen
        (
          if
            cfg.video.enable
          # then [obs-studio handbrake]
          then
            [ handbrake ]
          else
            [ ]
        );
  };
}
