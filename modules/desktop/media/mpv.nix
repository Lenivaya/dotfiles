{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.media.mpv;
in {
  options.modules.desktop.media.mpv.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.configFile."mpv" = {
      source = "${configDir}/mpv";
      recursive = true;
    };

    user.packages = with pkgs; let
      mpvPkg =
        mpv.override
        {
          scripts = with mpvScripts; [
            autoload
            cutter
            convert
            # thumbnail

            sponsorblock
            youtube-quality
            webtorrent-mpv-hook

            my.mpv-autosub
          ];
        };
    in [
      mpvPkg
      ytfzf # find and watch videos on youtube
    ];
  };
}
