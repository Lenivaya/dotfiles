{
  config,
  options,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.desktop.media.mpv;
  inherit (config.dotfiles) configDir;
in {
  options.modules.desktop.media.mpv.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.configFile."mpv" = {
      source = "${configDir}/mpv";
      recursive = true;
    };

    user.packages = with pkgs; [
      # mpv-with-scripts
      (
        mpv.override
        {
          scripts = with mpvScripts; [
            autoload
            cutter
            convert
            thumbnail
            sponsorblock
            youtube-quality
          ];
        }
      )

      mpvc # CLI controller for mpv
      (mkIf config.services.xserver.enable celluloid) # nice GTK GUI for mpv
    ];
  };
}
