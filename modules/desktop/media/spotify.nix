{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.spotify;
in {
  options.modules.media.spotify = { enable = mkBoolOpt false; };

  config = mkIf config.modules.media.spotify.enable {
    my.packages = with pkgs;
      [

        spotify
      ];
  };

}
