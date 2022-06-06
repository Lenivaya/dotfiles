{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.mpv;
  configDir = config.dotfiles.configDir;
in {
  options.modules.desktop.media.mpv.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    nixpkgs.overlays = mkIf config.modules.desktop.browsers.firefox.enable
      [ inputs.nur.overlay ];

    home.configFile."mpv" = {
      source = "${configDir}/mpv";
      recursive = true;
    };

    user.packages = with pkgs; [
      mpv-with-scripts
      mpvc # CLI controller for mpv
      (mkIf config.services.xserver.enable celluloid) # nice GTK GUI for mpv
    ];
  };
}
