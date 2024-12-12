# https://github.com/SpitFire-666/Firefox-Stuff
# https://codeberg.org/wolfangaukang/multifirefox ?
# https://wiki.nixos.org/wiki/Firefox
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

  readHack = path: ''
    ${readFile "${inputs.firefox-csshacks}/${path}"}
  '';
  userChrome = ''
    ${readHack "chrome/centered_statuspanel.css"}
    ${readHack "chrome/urlbar_centered_text.css"}
    ${readHack "chrome/compact_proton.css"}
    ${readHack "chrome/compact_urlbar_megabar.css"}
    ${readHack "chrome/minimal_in-UI_scrollbars.css"}

    ${readHack "chrome/hide_tabs_with_one_tab_w_window_controls.css"}
    ${readHack "chrome/numbered_tabs.css"}
    ${readHack "chrome/combined_favicon_and_tab_close_button.css"}
    ${readHack "chrome/tab_separator_lines.css"}
    ${readHack "chrome/dual_color_tab_attention_indicator.css"}

    ${readHack "chrome/autohide_bookmarks_and_main_toolbars.css"}
    ${readHack "chrome/hide_toolbox_top_bottom_borders.css"}
    ${readHack "chrome/less_static_throbber.css"}

    ${readHack "chrome/iconized_main_menu.css"}
    ${readHack "chrome/iconized_menubar_items.css"}
    ${readHack "chrome/iconized_places_context_menu.css"}
    ${readHack "chrome/iconized_tabs_context_menu.css"}
    ${readHack "chrome/iconized_textbox_context_menu.css"}
    ${readHack "chrome/iconized_content_context_menu.css"}
    ${readHack "chrome/icon_only_context_menu_text_controls.css"}

    ${readFile "${configDir}/firefox/userChrome.css"}
  '';
  userContent = ''
    ${readHack "content/compact_about_config.css"}
    ${readHack "content/compact_addons_manager.css"}
    ${readHack "content/limit_css_data_leak.css"}
    ${readFile "${configDir}/firefox/userContent.css"}
  '';

  settings = import "${configDir}/firefox/preferences.nix";
  searchEngines = import "${configDir}/firefox/search-engines.nix";

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
      don-t-fuck-with-paste
      tab-session-manager
      languagetool
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
    environment.sessionVariables = {
      MOZ_WEBRENDER = 1;
      MOZ_USE_XINPUT2 = "1";
    };

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

        search = {
          force = true;
          engines = searchEngines;
        };

        extraConfig = concatStringsSep "\n" [
          (readFile "${inputs.betterfox}/Fastfox.js")
          (readFile "${inputs.betterfox}/Securefox.js")
          (readFile "${inputs.betterfox}/Peskyfox.js")
        ];
      };
    };
  };
}
