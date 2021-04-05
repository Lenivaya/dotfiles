{ config, options, lib, pkgs, home-manager, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.browsers.firefox;
in {
  options.modules.desktop.browsers.firefox = {
    enable = mkBoolOpt false;
    profileName = mkOpt types.str config.user.name;
  };

  config = mkIf cfg.enable {

    # Desktop entry for private firefox window
    user.packages = with pkgs;
      [
        (makeDesktopItem {
          name = "firefox-private";
          desktopName = "Firefox (Private)";
          genericName = "Open a private Firefox window";
          icon = "firefox";
          exec = "firefox --private-window";
          categories = "Network";
        })
      ];

    env.XDG_DESKTOP_DIR = "$HOME"; # prevent firefox creating ~/Desktop

    home.file = {
      ".mozilla/firefox/profiles.ini".text = ''
        [Profile0]
        Name=default
        IsRelative=1
        Path=${cfg.profileName}.default
        Default=1

        [General]
        StartWithLastProfile=1
        Version=2
      '';
    };

    home-manager.users.${config.user.name}.programs.firefox = {
      enable = true;
      package = pkgs.firefox-bin;

      profiles.default = {
        settings = {
          "devtools.theme" = "dark";
          # Keep window opened when last tab closed
          "browser.tabs.closeWindowWithLastTab" = false;
          "browser.tabs.insertAfterCurrent" = true;
          "browser.tabs.loadBookmarksInTabs" = true;
          # Enable userContent.css and userChrome.css
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

          # Hardware-acceleration
          "layers.acceleration.force-enabled" = true;
          "layers.omtp.enabled" = true;
          "layout.display-list.retain" = true;
          "layout.display-list.retain.chrome" = true;

          # Do not check if Firefox is the default browser
          "browser.shell.checkDefaultBrowser" = false;
          # Disable the "new tab page" feature and show a blank tab instead
          # https://wiki.mozilla.org/Privacy/Reviews/New_Tab
          # https://support.mozilla.org/en-US/kb/new-tab-page-show-hide-and-customize-top-sites#w_how-do-i-turn-the-new-tab-page-off
          "browser.newtabpage.enabled" = false;
          "browser.newtab.url" = "about:blank";
          # Disable Activity Stream
          # https://wiki.mozilla.org/Firefox/Activity_Stream
          "browser.newtabpage.activity-stream.enabled" = false;
          # Disable new tab tile ads & preload
          # http://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
          # http://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
          # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
          # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
          # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
          "browser.newtabpage.enhanced" = false;
          "browser.newtab.preload" = false;
          "browser.newtabpage.directory.ping" = "";
          "browser.newtabpage.directory.source" = "data:text/plain,{}";
          # Disable some not so useful functionality.
          "extensions.htmlaboutaddons.recommendations.enabled" = false;
          "extensions.htmlaboutaddons.discover.enabled" = false;
          "extensions.pocket.enabled" = false;
          "app.normandy.enabled" = false;
          "app.normandy.api_url" = "";
          "extensions.shield-recipe-client.enabled" = false;
          "app.shield.optoutstudies.enabled" = false;
          # Disable battery API
          # https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager
          # https://bugzilla.mozilla.org/show_bug.cgi?id=1313580
          "dom.battery.enabled" = false;
          # Disable "beacon" asynchronous HTTP transfers (used for analytics)
          # https://developer.mozilla.org/en-US/docs/Web/API/navigator.sendBeacon
          "beacon.enabled" = false;
          # Disable pinging URIs specified in HTML <a> ping= attributes
          # http://kb.mozillazine.org/Browser.send_pings
          "browser.send_pings" = false;
          # Disable telemetry
          # https://wiki.mozilla.org/Platform/Features/Telemetry
          # https://wiki.mozilla.org/Privacy/Reviews/Telemetry
          # https://wiki.mozilla.org/Telemetry
          # https://www.mozilla.org/en-US/legal/privacy/firefox.html#telemetry
          # https://support.mozilla.org/t5/Firefox-crashes/Mozilla-Crash-Reporter/ta-p/1715
          # https://wiki.mozilla.org/Security/Reviews/Firefox6/ReviewNotes/telemetry
          # https://gecko.readthedocs.io/en/latest/browser/experiments/experiments/manifest.html
          # https://wiki.mozilla.org/Telemetry/Experiments
          # https://support.mozilla.org/en-US/questions/1197144
          # https://firefox-source-docs.mozilla.org/toolkit/components/telemetry/telemetry/internals/preferences.html#id1
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.archive.enabled" = false;
          "experiments.supported" = false;
          "experiments.enabled" = false;
          "experiments.manifest.uri" = "";
          # Disable health reports (basically more telemetry)
          # https://support.mozilla.org/en-US/kb/firefox-health-report-understand-your-browser-perf
          # https://gecko.readthedocs.org/en/latest/toolkit/components/telemetry/telemetry/preferences.html
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.healthreport.service.enabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
        };

        userChrome = ''
          /* @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul"); */

            @-moz-document url(chrome://browser/content/browser.xul),
                            url(chrome://browser/content/browser.xhtml)
            {

            /* Replace favicon on tabs with close button on mouse hover */
            .tabbrowser-tab:not(:hover) .tab-close-button,
            .tabbrowser-tab:not([pinned]):hover .tab-icon-image { display: none !important; }
            .tabbrowser-tab:not([pinned]):hover .tab-close-button { display:block !important; }

            .tabbrowser-tab:hover .tab-throbber,
            .tabbrowser-tab:hover .tab-icon-image,
            .tabbrowser-tab:hover .tab-sharing-icon-overlay,
            .tabbrowser-tab:hover .tab-icon-overlay,
            .tabbrowser-tab:hover .tab-label-container,
            .tabbrowser-tab:hover .tab-icon-sound {
              -moz-box-ordinal-group: 2 !important;
            }

            .tabbrowser-tab .tab-close-button {
              margin-left: -2px !important;
              margin-right: 4px !important;
            }

            .tab-close-button:hover {
              fill-opacity: 0 !important;
            }

            .tabbrowser-tab::after,
            .tabbrowser-tab::before {
              border-left: none !important;
              border-right: none !important;
            }

            .scrollbutton-up[orient="horizontal"]~spacer { border-width: 0px; opacity: 0 }
          }
        '';

        userContent = ''
          /* Hide scrollbar */

          :root{
            scrollbar-width: none !important;
          }

          @-moz-document url(about:privatebrowsing) {
            :root{
              scrollbar-width: none !important;
            }
          }
        '';
      };
    };
  };
}
