{ config, options, lib, pkgs, ... }:

with lib; {
  options.modules.desktop.browsers.firefox = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    profileName = mkOption {
      type = types.str;
      default = config.my.username;
    };
  };

  config = mkIf config.modules.desktop.browsers.firefox.enable {

    my.home.programs.firefox = {
      enable = true;
      package = pkgs.firefox;
    };

    my.packages = with pkgs;
      [
        (makeDesktopItem {
          name = "firefox-private";
          desktopName = "Firefox (Private)";
          genericName = "Open a private Firefox window";
          icon = "firefox";
          exec = "${firefox}/bin/firefox --private-window";
          categories = "Network";
        })
      ];

    my.env.XDG_DESKTOP_DIR = "$HOME"; # prevent firefox creating ~/Desktop

    my.home.home.file = let cfg = config.modules.desktop.browsers.firefox;
    in {
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

    my.home.programs.firefox.profiles.default = {
      settings = {
        "browser.tabs.closeWindowWithLastTab" = false;
        "browser.tabs.insertAfterCurrent" = true;
        "browser.tabs.loadBookmarksInTabs" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

        # Hardware-acceleration
        "layers.acceleration.force-enabled" = true;
        "layers.omtp.enabled" = true;
        "layout.display-list.retain" = true;
        "layout.display-list.retain.chrome" = true;
      };

      userChrome = ''
        /* @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul"); */

        @-moz-document url(chrome://browser/content/browser.xul),
                        url(chrome://browser/content/browser.xhtml)
        {
          /* Remove the colored overline on the focused tab */
          .tabbrowser-tab .tab-line { background: none !important; }

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
    };

  };
}
