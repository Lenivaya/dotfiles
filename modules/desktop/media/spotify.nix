{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib;
with lib.my;
let
  inherit (inputs) spicetify-nix;

  cfg = config.modules.desktop.media.spotify;
  spicePkgs = spicetify-nix.legacyPackages.${pkgs.stdenv.system};
in
{
  options.modules.desktop.media.spotify.enable = mkBoolOpt false;

  imports = [ spicetify-nix.homeManagerModules.default ];

  config = mkIf cfg.enable {
    programs.spicetify = enabled // {
      enabledCustomApps = with spicePkgs.apps; [
        lyricsPlus
      ];
      enabledExtensions = with spicePkgs.extensions; [
        copyLyrics
        autoSkipVideo
        hidePodcasts
        adblock
        shuffle
        featureShuffle
        betterGenres
        powerBar
        keyboardShortcut
        seekSong
        goToSong
        fullAlbumDate
        playlistIntersection
        copyToClipboard
        playingSource

        ({
          src = pkgs.fetchFromGitHub {
            owner = "ohitstom";
            repo = "spicetify-extensions";
            rev = "0fa2e593742011a29f8ec54c32bf642205ce82ff";
            hash = "sha256-w8viHMXhuXIlo1WJ94Jo2bWtYQr1RGviXZhVVm+rvns=";
          };
          name = "noControls/noControls.js";
        })
        ({
          src = pkgs.fetchFromGitHub {
            owner = "ohitstom";
            repo = "spicetify-extensions";
            rev = "0fa2e593742011a29f8ec54c32bf642205ce82ff";
            hash = "sha256-w8viHMXhuXIlo1WJ94Jo2bWtYQr1RGviXZhVVm+rvns=";
          };
          name = "immersiveView/immersiveView.js";
        })
        ({
          src = pkgs.fetchFromGitHub {
            owner = "adventuretc";
            repo = "Spicetify-HideImages-Extension";
            rev = "a47cf6ba0955c9812edb438f96c2abcd8848273a";
            hash = "sha256-FBVnV3OHRsEfhC5x9wqo0SPEvT5JkdDKf+MT+4DGFUk=";
          };
          name = "HideImages.js";
        })
        ({
          src = pkgs.fetchFromGitHub {
            owner = "5E7EN";
            repo = "spicetify-hide-pritzus";
            rev = "59b93991346ef731ed32e6e279ee3eef3395b6a8";
            hash = "sha256-tU+d4Y5gQICmbF4EOVzel91JWjHXPoP7hu9h+rQXfAg=";
          };
          name = "hidePritzus.js";
        })
      ];

      theme = spicePkgs.themes.text // {
        additionalCss = ''
          /* user settings */
          :root {
              --font-family: monospace;
              --font-size: 16px;
              --font-weight: 400; /* 200 : 900 */
              --line-height: 1.0;

              --font-size-lyrics: 14px; /* 1.5em (default) */

              --font-family-header: monospace;
              --font-size-multiplier-header: 4;

              --display-card-image: block; /* none | block */
              --display-coverart-image: none; /* none | block */
              --display-header-image: none; /* none | block */
              --display-library-image: block; /* none | block */
              --display-tracklist-image: none; /* none | block */
              --display-spicetify-banner-ascii: none; /* none | block */
              --display-music-banner-ascii: none; /* none | block */

              --border-radius: 0px;
              --border-width: 1px;
              --border-style: solid; /* dotted | dashed | solid | double | groove | ridge | inset | outset */
          }
        '';
      };
    };
  };
}
