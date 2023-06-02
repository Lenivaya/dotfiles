{
  "browser.uidensity" = 1; # minimal ui
  "devtools.theme" = "dark";
  # 'browser.uiCustomization.state'. This tells firefox to sync it between
  # machines. WARNING: This may not work across OSes. Since I use NixOS on
  # all the machines I use Firefox on, this is no concern to me.
  "services.sync.prefs.sync.browser.uiCustomization.state" = true;
  # Keep window opened when last tab closed
  "browser.tabs.closeWindowWithLastTab" = false;
  "browser.tabs.insertAfterCurrent" = true;
  "browser.tabs.insertRelatedAfterCurrent" = true;
  "browser.tabs.loadBookmarksInTabs" = true;
  # Enable userContent.css and userChrome.css
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  # Hide 'http://' from url
  "browser.urlbar.trimURLs" = true;
  "browser.compactmode.show" = true;
  # Start with blank page
  "browser.startup.page" = 0;

  # Pdf's
  # Don't download pdf's just for previews
  "browser.download.open_pdf_attachments_inline" = true;

  # display two pages at once with the integrated PDF viewer
  # also automatically make it one page view using wrapped
  # scrolling
  "pdfjs.defaultZoomValue" = "page-fit";
  "pdfjs.scrollModeOnLoad" = 2; # wrapped scrolling
  "pdfjs.spreadModeOnLoad" = 0; # no spreads
  # "pdfjs.spreadModeOnLoad" = 1;

  # Hardware-acceleration
  "layers.acceleration.force-enabled" = true;
  "layers.omtp.enabled" = true;
  "layout.display-list.retain" = true;
  "layout.display-list.retain.chrome" = true;
  "media.ffmpeg.vaapi.enabled" = true;
  "media.rdd-vpx.enabled" = true;
  "media.ffvpx.enabled" = true;
  "media.navigator.mediadatadecoder_vpx_enabled" = true;
  # "gfx.webrender.all" = true;

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
  "browser.newtabpage.activity-stream.telemetry" = false;
  # Disable new tab tile ads & preload
  # http://www.thewindowsclub.com/disable-remove-ad-tiles-from-firefox
  # http://forums.mozillazine.org/viewtopic.php?p=13876331#p13876331
  # https://wiki.mozilla.org/Tiles/Technical_Documentation#Ping
  # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-source
  # https://gecko.readthedocs.org/en/latest/browser/browser/DirectoryLinksProvider.html#browser-newtabpage-directory-ping
  "browser.newtabpage.enhanced" = false;
  "browser.newtabpage.introShown" = true;
  # "browser.newtab.preload" = false;
  "browser.newtabpage.directory.ping" = "";
  "browser.newtabpage.directory.source" = "data:text/plain,{}";

  # Reduce search engine noise in the urlbar's completion window. The
  # shortcuts and suggestions will still work, but Firefox won't clutter
  # its UI with reminders that they exist.
  "browser.urlbar.suggest.searches" = false;
  "browser.urlbar.shortcuts.bookmarks" = false;
  "browser.urlbar.shortcuts.history" = false;
  "browser.urlbar.shortcuts.tabs" = false;
  "browser.urlbar.showSearchSuggestionsFirst" = false;
  "browser.urlbar.speculativeConnect.enabled" = false;
  # https://bugzilla.mozilla.org/1642623
  "browser.urlbar.dnsResolveSingleWordsAfterSearch" = 0;
  # https://blog.mozilla.org/data/2021/09/15/data-and-firefox-suggest/
  "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
  "browser.urlbar.suggest.quicksuggest.sponsored" = false;

  # Enable some helpful features in urlbar
  "browser.urlbar.suggest.calculator" = true;
  "browser.urlbar.unitConversion.enabled" = true;

  # Disable some not so useful functionality.
  "browser.disableResetPrompt" =
    true; # "Looks like you haven't started Firefox in a while."
  "browser.onboarding.enabled" =
    false; # "New to Firefox? Let's get started!" tour
  "browser.aboutConfig.showWarning" =
    false; # Warning when opening about:config
  "extensions.pocket.enabled" = false;
  "extensions.shield-recipe-client.enabled" = false;

  # Security-oriented defaults
  "security.family_safety.mode" = 0;
  # https://blog.mozilla.org/security/2016/10/18/phasing-out-sha-1-on-the-public-web/
  "security.pki.sha1_enforcement_level" = 1;
  # https://github.com/tlswg/tls13-spec/issues/1001
  "security.tls.enable_0rtt_data" = false;
  # Use Mozilla geolocation service instead of Google if given permission
  "geo.provider.network.url" = "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";
  "geo.provider.use_gpsd" = false;
  # https://support.mozilla.org/en-US/kb/extension-recommendations
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr" = false;
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" =
    false;
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" =
    false;
  "extensions.htmlaboutaddons.recommendations.enabled" = false;
  "extensions.htmlaboutaddons.discover.enabled" = false;
  "extensions.getAddons.showPane" = false; # uses Google Analytics
  "browser.discovery.enabled" = false;
  # Reduce File IO / SSD abuse
  # Otherwise, Firefox bombards the HD with writes. Not so nice for SSDs.
  # This forces it to write every 30 minutes, rather than 15 seconds.
  "browser.sessionstore.interval" = "1800000";
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
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.server" = "data:,";
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.coverage.opt-out" = true;
  "toolkit.coverage.opt-out" = true;
  "toolkit.coverage.endpoint.base" = "";
  "experiments.supported" = false;
  "experiments.enabled" = false;
  "experiments.manifest.uri" = "";
  "browser.ping-centre.telemetry" = false;
  # https://mozilla.github.io/normandy/
  "app.normandy.enabled" = false;
  "app.normandy.api_url" = "";
  "app.shield.optoutstudies.enabled" = false;
  # Disable health reports (basically more telemetry)
  # https://support.mozilla.org/en-US/kb/firefox-health-report-understand-your-browser-perf
  # https://gecko.readthedocs.org/en/latest/toolkit/components/telemetry/telemetry/preferences.html
  "datareporting.healthreport.uploadEnabled" = false;
  "datareporting.healthreport.service.enabled" = false;
  "datareporting.policy.dataSubmissionEnabled" = false;
  # Disable firefox Accessibility Service
  # https://www.reddit.com/r/firefox/comments/p8g5zd/why_does_disabling_accessibility_services_improve/
  "accessibility.force_disabled" = 1;
  # Enable ETP for decent security (makes firefox containers and many
  # common security/privacy add-ons redundant).
  "browser.contentblocking.category" = "strict";
  "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  "privacy.purge_trackers.enabled" = true;

  # disable reports
  "browser.crashReports.unsubmittedCheck.autoSubmit" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit2" =
    false; # don't submit backlogged reports
  "browser.crashReports.unsubmittedCheck.enabled" = false;
  "browser.tabs.crashReporting.sendReport" = false;
  "breakpad.reportURL" = "";
}
