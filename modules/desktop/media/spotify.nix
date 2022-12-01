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
        version = "2.14.3";
        src = pkgs.fetchgit {
          url = "https://github.com/spicetify/${pname}";
          rev = "v${version}";
          sha256 = "sha256-7bCl8VfkMhoTBnr+O+oBYQeSV2sRwlP/qUkNkYerZdU=";
        };
      });
      # theme = "SpotifyNoPremium";
      theme = "Nord";

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
