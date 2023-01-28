{
  config,
  options,
  lib,
  pkgs,
  inputs,
  home-manager,
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

    # Desktop entry for private firefox window
    user.packages = with pkgs; [
      (makeDesktopItem {
        name = "firefox-private";
        desktopName = "Firefox (Private)";
        genericName = "Open a private Firefox window";
        icon = "firefox";
        exec = "firefox --private-window";
        categories = ["Network"];
      })
    ];

    env.XDG_DESKTOP_DIR = "$HOME"; # prevent firefox creating ~/Desktop

    home.programs.firefox = {
      enable = true;
      package = pkgs.firefox.override {
        extraNativeMessagingHosts = with pkgs; [
          # Watch videos using mpv
          nur.repos.ambroisie.ff2mpv-go
        ];
      };
      # package = pkgs.firefox-esr;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        sponsorblock
        ublock-origin
        privacy-badger
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
        octotree
        reddit-comment-collapser
        reddit-enhancement-suite
      ];

      profiles.default = with builtins; {
        settings = import "${configDir}/firefox/preferences.nix";

        userChrome = readFile "${configDir}/firefox/userChrome.css";
        userContent = readFile "${configDir}/firefox/userContent.css";
      };
    };
  };
}
