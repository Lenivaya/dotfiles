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
  inherit (config) modules;

  cfg = config.modules.desktop.browsers.firefox;
  firefoxExtensions = pkgs.nur.repos.rycee.firefox-addons;

  userChrome = readFile "${configDir}/firefox/userChrome.css";
  userContent = readFile "${configDir}/firefox/userContent.css";
  settings = import "${configDir}/firefox/preferences.nix";
  extensions = with firefoxExtensions;
    [
      vimium-c

      sponsorblock
      ublock-origin
      libredirect
      buster-captcha-solver
      # bypass-paywalls-clean
      # clearurls

      terms-of-service-didnt-read
      istilldontcareaboutcookies # i-dont-care-about-cookies
      consent-o-matic

      tree-style-tab
      tst-tab-search
      tab-session-manager

      multi-account-containers
      violentmonkey

      # darkreader

      refined-github
      reddit-comment-collapser
      reddit-enhancement-suite
      enhancer-for-youtube
    ]
    ++ optional modules.desktop.media.mpv.enable ff2mpv
    ++ optional modules.shell.pass.enable passff;
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

    # Smooth scrolling
    environment.sessionVariables.MOZ_USE_XINPUT2 = "1";

    home.programs.firefox =
      enabled
      // {
        package = with pkgs; let
          firefox' = firefox.override {
            extraPolicies = {
              DisableAppUpdate = true;
              DisableFirefoxStudies = true;
              DisableTelemetry = true;
              DisablePocket = true;
              DontCheckDefaultBrowser = true;
              CaptivePortal = false;
              HardwareAcceleration = true;
            };
            extraNativeMessagingHosts = with pkgs;
              []
              ++ optional modules.desktop.media.mpv.enable my.ff2mpv-rust
              ++ optional modules.shell.pass.enable passff-host;
          };
        in
          firefox';

        profiles.default = {
          id = 0;
          inherit settings extensions userChrome userContent;

          extraConfig = concatStringsSep "\n" [
            (readFile "${inputs.betterfox}/Fastfox.js")
            (readFile "${inputs.betterfox}/Peskyfox.js")
            (readFile "${inputs.betterfox}/user.js")
          ];
        };
      };
  };
}
