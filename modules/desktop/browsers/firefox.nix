# https://github.com/SpitFire-666/Firefox-Stuff
# https://codeberg.org/wolfangaukang/multifirefox ?
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

      terms-of-service-didnt-read
      # consent-o-matic
      don-t-fuck-with-paste

      # tree-style-tab
      # tst-tab-search
      tab-session-manager

      multi-account-containers
      violentmonkey

      refined-github
      reddit-comment-collapser
      reddit-enhancement-suite
    ]
    ++ optional modules.desktop.media.mpv.enable ff2mpv
    ++ optional modules.shell.pass.enable passff;

  firefox' = cfg.package.override {
    extraPolicies = {
      # https://github.com/mozilla/policy-templates/blob/master/linux/policies.json
      # about:policies#documentation
      ManualAppUpdateOnly = true;
      AppAutoUpdate = false;
      DisableAppUpdate = true;
      DisableFeedbackCommands = true;
      DisableFirefoxStudies = true;
      DisableTelemetry = true;
      DisablePocket = true;
      DisableSetDesktopBackground = true;
      DontCheckDefaultBrowser = true;
      CaptivePortal = false;
      HardwareAcceleration = true;
      UserMessaging = {
        ExtensionRecommendations = false;
        FeatureRecommendations = false;
        MoreFromMozilla = false;
        SkipOnboarding = true;
        UrlbarInterventions = false;
        WhatsNew = false;
      };
      OverrideFirstRunPage = "";
      OverridePostUpdatePage = "";
      ShowHomeButton = false;
      PromptForDownloadLocation = true;
      FirefoxHome = {
        Search = true;
        Pocket = false;
        Snippets = false;
        TopSites = false;
        Highlights = false;
        SponsoredTopSites = false;
      };
      FirefoxSuggest = {
        WebSuggestions = false;
        SponsoredSuggestions = false;
        ImproveSuggest = false;
      };
    };
    nativeMessagingHosts =
      with pkgs;
      optional modules.desktop.media.mpv.enable ff2mpv-rust
      ++ optional modules.shell.pass.enable passff-host;
  };
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
      package = firefox';

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
        ];
      };
    };
  };
}
