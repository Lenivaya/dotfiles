# https://github.com/SpitFire-666/Firefox-Stuff
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
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.browsers.firefox;
in {
  options.modules.desktop.browsers.firefox.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    nixpkgs.overlays = [inputs.nur.overlay];

    env.XDG_DESKTOP_DIR = "$HOME"; # prevent firefox creating ~/Desktop

    user.packages = with pkgs; [
      profile-cleaner

      (makeDesktopItem {
        name = "firefox-private";
        desktopName = "Firefox (Private)";
        genericName = "Open a private Firefox window";
        icon = "firefox";
        exec = "firefox --private-window";
        categories = ["Network"];
      })
    ];

    home.programs.firefox =
      enabled
      // {
        package = pkgs.firefox.override {
          extraNativeMessagingHosts = with pkgs; [
            my.ff2mpv-rust
          ];
        };

        profiles.default = with builtins; {
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            vimium-c

            sponsorblock
            ublock-origin
            localcdn # decentraleyes
            clearurls
            libredirect
            # terms-of-service-didnt-read
            buster-captcha-solver

            tree-style-tab
            tab-session-manager

            ff2mpv
            h264ify

            violentmonkey

            refined-github
            reddit-comment-collapser
            reddit-enhancement-suite
          ];

          settings = import "${configDir}/firefox/preferences.nix";
          userChrome = readFile "${configDir}/firefox/userChrome.css";
          userContent = readFile "${configDir}/firefox/userContent.css";
        };
      };
  };
}
