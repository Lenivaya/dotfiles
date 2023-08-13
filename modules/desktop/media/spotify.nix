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
    programs.spicetify =
      enabled
      // {
        # theme = spicePkgs.themes.Default;

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
          # wikify
          adblock
          # charliesAdblock

          groupSession
        ];
      };
  };
}
