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
  cfg = config.modules.desktop.media.spotify;
  inherit (inputs) spicetify-nix;
in {
  options.modules.desktop.media.spotify.enable = mkBoolOpt false;

  imports = [spicetify-nix.homeManagerModule];

  config = mkIf cfg.enable {
    programs.spicetify = {
      enable = true;
      spotifyPackage = pkgs.unstable.spotify-unwrapped;
      spicetifyPackage = pkgs.spicetify-cli.overrideAttrs (oa: rec {
        pname = "spicetify-cli";
        version = "2.9.9";
        src = pkgs.fetchgit {
          url = "https://github.com/spicetify/${pname}";
          rev = "v${version}";
          sha256 = "1a6lqp6md9adxjxj4xpxj0j1b60yv3rpjshs91qx3q7blpsi3z4z";
        };
      });
      theme = "SpotifyNoPremium";

      enabledCustomApps = with spicetify-nix.pkgs.apps; [
        lyrics-plus
        localFiles
        marketplace
      ];
      enabledExtensions = [
        "powerBar.js"
        "keyboardShortcut.js"
        "seekSong.js"
        "goToSong.js"

        "fullAppDisplayMod.js" #"fullAppDisplay.js"
        "shuffle+.js"
        # "hidePodcasts.js"
        "fullAlbumDate.js"
        "playlistIcons.js"
        "wikify.js"
        "adblock.js"

        "wikify.js"
        "groupSession.js"
      ];
    };
  };
}
