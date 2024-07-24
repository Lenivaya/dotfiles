{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my; let
  inherit (inputs) spicetify-nix;

  cfg = config.modules.desktop.media.spotify;
  spicePkgs = spicetify-nix.legacyPackages.${pkgs.stdenv.system};
in {
  options.modules.desktop.media.spotify.enable = mkBoolOpt false;

  imports = [spicetify-nix.homeManagerModules.default];

  config = mkIf cfg.enable {
    programs.spicetify =
      enabled
      // {
        # theme = spicePkgs.themes.text;
        theme =
          spicePkgs.themes.spotifyNoPremium;

        enabledCustomApps = with spicePkgs.apps; [
          lyricsPlus
          localFiles
          # marketplace
        ];
        enabledExtensions = with spicePkgs.extensions; [
          adblock

          shuffle
          history
          betterGenres
          # beautifulLyrics

          powerBar
          keyboardShortcut
          seekSong
          goToSong

          fullAppDisplayMod
          fullAlbumDate
          playlistIcons
          # wikify

          groupSession
        ];
      };
  };
}
