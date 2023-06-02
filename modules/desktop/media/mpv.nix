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
  mpvPkg = with pkgs;
    mpv.override
    {
      scripts = with mpvScripts; [
        autoload
        cutter
        convert
        # thumbnail
        sponsorblock
        youtube-quality
      ];
    };
in {
  options.modules.desktop.media.mpv.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    home.configFile."mpv" = {
      source = "${configDir}/mpv";
      recursive = true;
    };

    user.packages = with pkgs; [
      mpvPkg

      # mpvc # CLI controller for mpv
      # (mkIf config.services.xserver.enable celluloid) # nice GTK GUI for mpv
    ];
  };
}
