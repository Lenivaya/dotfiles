# https://github.com/SpitFire-666/Firefox-Stuff
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
  inherit (config.dotfiles) configDir;
  inherit (config) modules;

  cfg = config.modules.desktop.browsers.firefox;
  firefoxExtensions = pkgs.nur.repos.rycee.firefox-addons;

  userChrome = ''
    ${readFile "${configDir}/firefox/userChrome.css"}
    ${readFile "${inputs.minimalisticfox}/userChrome.css"}
  '';
  # ${readFile "${inputs.penguin-fox}/files/chrome/userChrome.css"}
  userContent = ''
    ${readFile "${configDir}/firefox/userContent.css"}
    ${readFile "${inputs.minimalisticfox}/userContent.css"}
  '';
  # ${readFile "${inputs.penguin-fox}/files/chrome/userContent.css"}
  settings = import "${configDir}/firefox/preferences.nix";
  extensions =
    with firefoxExtensions;
    [
      vimium-c

      sponsorblock
      ublock-origin
      libredirect
      buster-captcha-solver
      # bypass-paywalls-clean
      # clearurls

      terms-of-service-didnt-read
      consent-o-matic
      don-t-fuck-with-paste

      # Might be slowing down the pages a lot [1],
      # it's better to use ublock-origin with filter list
      # or even more simple inbuilt firefox
      # cookiebanners.service.mode 1
      # cookiebanners.service.mode.privateBrowsing 1
      #
      # [1]: https://www.reddit.com/r/firefox/comments/1dt5yte/you_should_know_the_extension_still_dont_care
      #
      # istilldontcareaboutcookies # i-dont-care-about-cookies

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
in
{
  options.modules.desktop.browsers.firefox = with types; {
    enable = mkBoolOpt false;
    package = mkOpt package pkgs.firefox;
    executable = mkOpt str "firefox";
  };

  config = mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.nur.overlay ];

    env.XDG_DESKTOP_DIR = "$HOME"; # prevent firefox creating ~/Desktop

    user.packages = with pkgs; [
      profile-cleaner

      (makeDesktopItem {
        name = "firefox-private";
        desktopName = "Firefox (Private)";
        genericName = "Open a private Firefox window";
        icon = "firefox";
        exec = "${cfg.executable} --private-window";
        categories = [ "Network" ];
      })
    ];

    # Smooth scrolling
    environment.sessionVariables.MOZ_USE_XINPUT2 = "1";

    home.programs.firefox = enabled // {
      package =
        with pkgs;
        let
          firefox' = cfg.package.override {
            extraPolicies = {
              DisableAppUpdate = true;
              DisableFirefoxStudies = true;
              DisableTelemetry = true;
              DisablePocket = true;
              DisableSetDesktopBackground = true;
              DontCheckDefaultBrowser = true;
              CaptivePortal = false;
              HardwareAcceleration = true;
              UserMessaging = {
                ExtensionRecommendations = false;
                SkipOnboarding = true;
              };
              OverrideFirstRunPage = "";
              PromptForDownloadLocation = true;
              FirefoxHome = {
                Search = true;
                Pocket = false;
                Snippets = false;
                TopSites = false;
                Highlights = false;
              };
            };
            nativeMessagingHosts =
              with pkgs;
              optional modules.desktop.media.mpv.enable ff2mpv-rust
              ++ optional modules.shell.pass.enable passff-host;
          };
        in
        firefox';

      profiles.default = {
        id = 0;
        inherit
          settings
          extensions
          userChrome
          userContent
          ;

        extraConfig = concatStringsSep "\n" [
          (readFile "${inputs.betterfox}/Fastfox.js")
          (readFile "${inputs.betterfox}/Securefox.js")
          (readFile "${inputs.betterfox}/Peskyfox.js")
          (readFile "${inputs.betterfox}/user.js")
        ];
      };
    };
  };
}
