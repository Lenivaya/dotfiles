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

  # I'd like to place my settings after extraConfig to override some values from betterfox and etc modules I use
  userPrefValue =
    pref:
    builtins.toJSON (if isBool pref || isInt pref || isString pref then pref else builtins.toJSON pref);
  mkUserJs = prefs: ''
    // My settings.
    ${concatStrings (
      mapAttrsToList (name: value: ''
        user_pref("${name}", ${userPrefValue value});
      '') prefs
    )}
  '';

  readHack = path: ''
    ${readFile "${inputs.firefox-csshacks}/${path}"}
  '';
  readChromeHack = path: ''
    ${readHack "chrome/${path}"}
  '';
  readContentHack = path: ''
    ${readHack "content/${path}"}
  '';
  userChrome = ''
    ${readChromeHack "hide_statuspanel_when_fullscreen.css"}
    ${readChromeHack "centered_statuspanel.css"}

    ${readChromeHack "compact_proton.css"}
    ${readChromeHack "minimal_in-UI_scrollbars.css"}
    ${readChromeHack "classic_grid_main_menu_popup.css"}

    ${readChromeHack "compact_urlbar_megabar.css"}
    ${readChromeHack "urlbar_centered_text.css"}

    ${readChromeHack "hide_tabs_with_one_tab_w_window_controls.css"}
    ${readChromeHack "numbered_tabs.css"}
    ${readChromeHack "combined_favicon_and_tab_close_button.css"}
    ${readChromeHack "tab_separator_lines.css"}
    ${readChromeHack "dual_color_tab_attention_indicator.css"}

    ${readChromeHack "autohide_bookmarks_and_main_toolbars.css"}
    ${readChromeHack "hide_toolbox_top_bottom_borders.css"}
    ${readChromeHack "less_static_throbber.css"}

    ${readChromeHack "iconized_main_menu.css"}
    ${readChromeHack "iconized_menubar_items.css"}
    ${readChromeHack "iconized_places_context_menu.css"}
    ${readChromeHack "iconized_tabs_context_menu.css"}
    ${readChromeHack "iconized_textbox_context_menu.css"}
    ${readChromeHack "iconized_content_context_menu.css"}
    ${readChromeHack "icon_only_context_menu_text_controls.css"}

    ${readChromeHack "grid_overflow_menu.css"}

    ${readFile "${configDir}/firefox/userChrome.css"}
  '';
  userContent = ''
    ${readContentHack "compact_about_config.css"}
    ${readContentHack "compact_addons_manager.css"}
    ${readContentHack "limit_css_data_leak.css"}
    ${readContentHack "remove_textbox_focusring.css"}
    ${readContentHack "transparent_reader_toolbar.css"}
    ${readContentHack "standalone_image_page_mods.css"}
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
      # buster-captcha-solver
      # bypass-paywalls-clean
      # terms-of-service-didnt-read
      don-t-fuck-with-paste
      tab-session-manager
      languagetool
      multi-account-containers
      violentmonkey
      refined-github
      # reddit-comment-collapser
      # reddit-enhancement-suite
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
    nixpkgs.overlays = [ inputs.nur.overlays.default ];

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

    environment.sessionVariables = {
      MOZ_WEBRENDER = 1;
      MOZ_USE_XINPUT2 = "1";
    };

    home.programs.firefox = enabled // {
      package = firefox';

      profiles.default = {
        id = 0;
        inherit
          # settings
          extensions
          userChrome
          userContent
          ;

        search = {
          force = true;
          default = "Google";
          order = [
            "Google"
            "DuckDuckGo"
            "Brave"
          ];
          engines = searchEngines;
        };

        extraConfig =
          let
            settings' = mkUserJs settings;
          in
          concatStringsSep "\n" [
            (readFile "${inputs.betterfox}/Fastfox.js")
            (readFile "${inputs.betterfox}/Securefox.js")
            (readFile "${inputs.betterfox}/Peskyfox.js")
            settings'
          ];
      };
    };
  };
}
