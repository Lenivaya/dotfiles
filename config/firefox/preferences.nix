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
  # don't use system theme
  "browser.theme.content-theme" = 2;

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
  "layers.mlgpu.enabled" = true;
  "media.ffmpeg.vaapi.enabled" = true;
  # "media.rdd-vpx.enabled" = true;
  "media.rdd-vpx.enabled" = false; # https://bugzilla.mozilla.org/show_bug.cgi?id=1673184
  # "media.ffvpx.enabled" = true;
  "media.ffvpx.enabled" = false; # https://www.opennet.ru/opennews/art.shtml?num=53086
  "media.navigator.mediadatadecoder_vpx_enabled" = true;
  "gfx.webrender.all" = true;
  "gfx.webrender.compositor.force-enabled" = true;
  "gfx.x11-egl.force-enabled" = true;
  "media.hardware-video-decoding.force-enabled" = true;

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
  "browser.disableResetPrompt" = true; # "Looks like you haven't started Firefox in a while."
  "browser.onboarding.enabled" = false; # "New to Firefox? Let's get started!" tour
  "browser.aboutConfig.showWarning" = false; # Warning when opening about:config
  "extensions.pocket.enabled" = false;
  "extensions.shield-recipe-client.enabled" = false;

  # Security-oriented defaults
  "security.family_safety.mode" = 0;
  # https://blog.mozilla.org/security/2016/10/18/phasing-out-sha-1-on-the-public-web/
  "security.pki.sha1_enforcement_level" = 1;
  # https://github.com/tlswg/tls13-spec/issues/1001
  "security.tls.enable_0rtt_data" = false;
  "geo.provider.use_gpsd" = false;
  # https://support.mozilla.org/en-US/kb/extension-recommendations
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr" = false;
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
  "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
  "extensions.htmlaboutaddons.recommendations.enabled" = false;
  "extensions.htmlaboutaddons.discover.enabled" = false;
  "extensions.getAddons.showPane" = false; # uses Google Analytics
  "browser.discovery.enabled" = false;
  # Reduce File IO / SSD abuse
  # Otherwise, Firefox bombards the HD with writes. Not so nice for SSDs.
  # This forces it to write every 30 minutes, rather than 15 seconds.
  # "browser.sessionstore.interval" = "1800000";
  # Disable battery API
  # https://developer.mozilla.org/en-US/docs/Web/API/BatteryManager
  # https://bugzilla.mozilla.org/show_bug.cgi?id=1313580
  "dom.battery.enabled" = false;
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
  # "browser.contentblocking.category" = "strict";
  # "privacy.donottrackheader.enabled" = true;
  # "privacy.donottrackheader.value" = 1;
  # "privacy.purge_trackers.enabled" = true;

  # disable reports
  "browser.crashReports.unsubmittedCheck.autoSubmit" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit2" = false; # don't submit backlogged reports
  "browser.crashReports.unsubmittedCheck.enabled" = false;
  "browser.tabs.crashReporting.sendReport" = false;
  "breakpad.reportURL" = "";

  # Allow svgs to take on theme colors
  "svg.context-properties.content.enabled" = true;

  # Enable color-mixing for the layout.css.color-mix.enabled flag
  "layout.css.color-mix.enabled" = true;

  # Seriously. Stop popping up on every damn page. If I want it translated,
  # I know where to find gtranslate/deepl/whatever!
  # "browser.translations.automaticallyPopup" = false;

  # enable HEVC
  "media.wmf.hevc.enabled" = true;

  # obsidian clipper
  "extensions.openPopupWithoutUserGesture.enabled" = true;

  # Tab groups
  "browser.tabs.groups.enabled" = true;

  "app.normandy.first_run" = false;
  "trailhead.firstrun.didSeeAboutWelcome" = true;
  "browser.uitour.enabled" = false;

  "layout.css.has-selector.enabled" = true;

  # https://www.reddit.com/r/firefox/comments/xqmlbt/does_anyone_know_how_to_reduce_animation_on/
  "ui.prefersReducedMotion" = 1;
  "toolkit.cosmeticAnimations.enable" = false;
  "full-screen-api.warning.delay" = 50;
  "full-screen-api.warning.timeout" = 50;
  "full-screen-api.transition-duration.enter" = "0 0";
  "full-screen-api.transition-duration.leave" = "0 0";
  "full-screen-api.transition.timeout" = 0;

  # Get rid of pointless URL bar element space-waster
  "browser.urlbar.hideGoButton" = true;

  # *Always* present the Reader Mode button in the toolbar, even if the automated parsing thinks it won't work
  "reader.parse-on-load.force-enable" = true;

  "image.jxl.enabled" = true;

  # https://discourse.mozilla.org/t/solved-window-decoration-theme-on-linux/67006/6
  # Disable this ugly as shit strange decorations that apperas like a border around my window on XMonad
  "browser.tabs.inTitlebar" = 0;

  "network.notify.checkForProxies" = true;

  "toolkit.cosmeticAnimations.enabled" = false;
  "toolkit.scrollbox.smoothScroll" = false;

  # https://www.reddit.com/r/firefox/comments/1ihjxn1/man_firefox_autocomplete/
  # "browser.urlbar.autoFill.adaptiveHistory.enabled" = true;

  # Disable those strange search sounds that annoy me
  # https://support.mozilla.org/ms/questions/1310139
  "accessibility.typeaheadfind.enablesound" = false;
  "accessibility.typeaheadfind.soundURL" = "beep";

  # https://www.reddit.com/r/firefox/comments/1inqy98/firefox_forkserver_getting_ready_to_enhance_linux/
  # https://www.phoronix.com/news/Firefox-ForkServer-Linux-Nears
  "dom.ipc.forkserver.enable" = true;

  "browser.urlbar.maxRichResults" = 20;

  # https://developer.mozilla.org/en-US/docs/Web/API/navigator.sendBeacon
  "beacon.enabled" = true; # breaks some sites like perplexity if disabled, lets just keep it enabled

  # // PREF: default permission for Web Notifications
  # // To add site exceptions: Page Info>Permissions>Receive Notifications
  # // To manage site exceptions: Options>Privacy & Security>Permissions>Notifications>Settings
  # // 0=always ask (default), 1=allow, 2=block
  "permissions.default.desktop-notification" = 1;

  ####
  #### https://gist.github.com/RubenKelevra/fd66c2f856d703260ecdf0379c4f59db
  ####
  # General tweaks
  "network.captive-portal-service.enabled" = false; # don't try to find captive portals
  # "network.notify.checkForProxies" = false; # don't try to find proxies
  "browser.cache.disk.enable" = true;
  "browser.cache.frecency_half_life_hours" = 18; # lower cache sweep intervals
  "browser.cache.max_shutdown_io_lag" = 16; # let the browser finish more io on shutdown
  "browser.cache.memory.capacity" = 2097152; # fixed maximum 2 GB in memory cache
  "browser.cache.memory.max_entry_size" = 327680; # maximum size of in memory cached objects
  "browser.cache.disk.metadata_memory_limit" = 15360; # increase size (in KB) of "Intermediate memory caching of frequently used metadata (a.k.a. disk cache memory pool)"
  # GFX rendering tweaks:
  "gfx.canvas.accelerated" = true;
  "gfx.canvas.accelerated.cache-items" = 32768;
  "gfx.canvas.accelerated.cache-size" = 4096;
  # "layers.acceleration.force-enabled" = false;
  "gfx.content.skia-font-cache-size" = 80;
  # "gfx.webrender.all" = true;
  "gfx.webrender.compositor" = true;
  # "gfx.webrender.compositor.force-enabled" = true;
  "gfx.webrender.enabled" = true;
  "gfx.webrender.precache-shaders" = true;
  "gfx.webrender.program-binary-disk" = true;
  "gfx.webrender.software.opengl" = true;
  "image.mem.decode_bytes_at_a_time" = 65536;
  "image.mem.shared.unmap.min_expiration_ms" = 120000;
  "layers.gpu-process.enabled" = true;
  "layers.gpu-process.force-enabled" = true;
  "image.cache.size" = 10485760;
  "media.memory_cache_max_size" = 1048576;
  "media.memory_caches_combined_limit_kb" = 3145728;
  # "media.hardware-video-decoding.force-enabled" = true;
  # "media.ffmpeg.vaapi.enabled" = true;
  # Increase predictive network operations
  "network.dns.disablePrefetchFromHTTPS" = false;
  "network.dnsCacheEntries" = 20000;
  "network.dnsCacheExpiration" = 3600;
  "network.dnsCacheExpirationGracePeriod" = 240;
  "network.predictor.enable-hover-on-ssl" = true;
  "network.predictor.enable-prefetch" = true;
  "network.predictor.preconnect-min-confidence" = 20;
  "network.predictor.prefetch-force-valid-for" = 3600;
  "network.predictor.prefetch-min-confidence" = 30;
  "network.predictor.prefetch-rolling-load-count" = 120;
  "network.predictor.preresolve-min-confidence" = 10;
  # Faster SSL
  "network.ssl_tokens_cache_capacity" = 32768; # more TLS token caching (fast reconnects)

  # "fission.autostart" = false; # disable enhanced protection between threads/processes of the browser (which uses LOADS of memory to do)
  # "privacy.partition.network_state" = false; # don't seperate the network state (e.g. the cache) by top level domains)

  # "dom.ipc.processCount" = 1;
  # "dom.ipc.processCount.webIsolated" = 1;

  # i don't really care
  "browser.contentblocking.category" = "custom";
  # "browser.contentblocking.fingerprinting.preferences.ui.enabled" = false;
  # Disable all custom filters in Privacy & Security -> Custom (Tracking Protection)
  # Disable Block dangerous and deceptive content in Security section
  "browser.safebrowsing.phishing.enabled" = false;
  "browser.safebrowsing.malware.enabled" = false;
  "browser.safebrowsing.downloads.enabled" = false;
  "browser.safebrowsing.downloads.remote.enabled" = false;
  "browser.safebrowsing.blockedURIs.enabled" = false;
  # Disable Enhanced Tracking Protection
  "privacy.trackingprotection.enabled" = false; # Main ETP switch
  "privacy.trackingprotection.pbmode.enabled" = false; # ETP in private browsing
  "privacy.trackingprotection.socialtracking.enabled" = false; # Social tracking
  # Disable individual protections
  "privacy.trackingprotection.cryptomining.enabled" = false;
  "privacy.trackingprotection.fingerprinting.enabled" = false;
  "privacy.trackingprotection.fingerprinting.annotate.enabled" = false;
  "privacy.trackingprotection.cryptomining.annotate.enabled" = false;
  # Disable cookie restrictions
  "network.cookie.cookieBehavior" = 0; # 0 = Accept all cookies
}
