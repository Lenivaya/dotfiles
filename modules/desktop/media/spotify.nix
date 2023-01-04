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
  inherit (inputs) spicetify-nix;

  cfg = config.modules.desktop.media.spotify;
  spicePkgs = spicetify-nix.packages.${pkgs.system}.default;
in {
  options.modules.desktop.media.spotify.enable = mkBoolOpt false;

  imports = [spicetify-nix.homeManagerModule];

  config = mkIf cfg.enable {
    programs.spicetify = {
      enable = true;

      spotifyPackage = pkgs.unstable.spotify-unwrapped;
      spicetifyPackage = pkgs.spicetify-cli.overrideAttrs (oa: rec {
        pname = "spicetify-cli";
        version = "2.15.0";
        src = pkgs.fetchgit {
          url = "https://github.com/spicetify/${pname}";
          rev = "v${version}";
          sha256 = "sha256-o1vzhD24chec4orMHoCJ8mHsEDhcX8zz3BJ6VzfT/NE=";
        };
      });

      # theme = "SpotifyNoPremium";
      theme = spicePkgs.themes.Nord;
      # theme = spicePkgs.themes.SpotifyNoPremium;

      enabledCustomApps = with spicePkgs.apps; [
        lyrics-plus
        localFiles
        marketplace
      ];
      enabledExtensions = with spicePkgs.extensions; [
        powerBar
        keyboardShortcut
        seekSong
        goToSong

        fullAppDisplayMod
        fullAlbumDate
        playlistIcons
        wikify
        # adblock
        # charliesAdblock

        groupSession
      ];
    };
  };
}
