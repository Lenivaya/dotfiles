{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.spotify;
in
{
  options.modules.desktop.media.spotify.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.nur.overlay ];
    user.packages = with pkgs; [
      # spotify
      spicetify-cli
      (writeScriptBin "spotify" ''
        #!${stdenv.shell}
        exec ${nur.repos.milahu.spotify-adblock-linux}/bin/spotify-adblock-linux
      '')
    ];
  };

}
