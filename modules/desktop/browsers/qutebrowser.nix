# modules/browser/qutebrowser.nix --- https://github.com/qutebrowser/qutebrowser
#
# Qutebrowser is cute because it's not enough of a browser to be handsome.
# Still, we can all tell he'll grow up to be one hell of a lady-killer.
{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my;
let
  cfg = config.modules.desktop.browsers.qutebrowser;
  pkg = pkgs.qutebrowser;
in
{
  options.modules.desktop.browsers.qutebrowser = with types; {
    enable = mkBoolOpt false;
    dicts = mkOpt (listOf str) [
      "en-US"
      "en-GB"
      "uk-UA"
      "ru-RU"
    ];
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      pkg
      (makeDesktopItem {
        name = "qutebrowser-private";
        desktopName = "Qutebrowser (Private)";
        genericName = "Open a private Qutebrowser window";
        icon = "qutebrowser";
        exec = "${getExe pkg} -T -s content.private_browsing true";
        categories = [ "Network" ];
      })
      # For Brave adblock in qutebrowser, which is significantly better than the
      # built-in host blocking. Works on youtube and crunchyroll ads!
      python312Packages.adblock
    ];

    # home = {
    #   configFile = {
    #     "qutebrowser" = {
    #       source = "${configDir}/qutebrowser";
    #       recursive = true;
    #     };
    #   };
    # };
    #
    home.programs.qutebrowser = enabled // {
      keyBindings.normal = mkIf config.modules.desktop.media.mpv.enable {
        "z" = "hint links spawn --detach mpv {hint-url}";
        "K" = "tab-next";
        "J" = "tab-prev";
      };

      searchEngines = rec {
        aliexpress = "https://www.aliexpress.com/wholesale?SearchText={}";
        ansible = "https://galaxy.ansible.com/search?keywords={}";
        arch = "https://wiki.archlinux.org/?search={}";
        crates = "https://crates.io/search?q={}";
        discogs = "https://www.discogs.com/search/?q={}";
        dockerhub = "https://hub.docker.com/search?q={}";
        doublegis = "https://2gis.ru/search/{}";
        duckduckgo = "https://duckduckgo.com/?q={}'";
        ecosia = "https://www.ecosia.org/search?q={}";
        factorio = "https://wiki.factorio.com/index.php?search={}";
        genius = "https://genius.com/search?q={}";
        github = "https://github.com/search?q={}";
        godocs = "https://godocs.io/?q={}";
        gogdb = "https://www.gogdb.org/products?search={}";
        google = "https://www.google.com/search?q={}";
        google-images = "https://www.google.com/search?q={}&tbm=isch";
        gopkgs = "https://pkg.go.dev/search?q={}";
        habr = "https://habr.com/ru/search/?q={}";
        hackage = "https://hackage.haskell.org/packages/search?terms={}";
        hackernews = "https://hn.algolia.com/?q={}";
        headhunter = "https://hh.ru/search/vacancy?st=searchVacancy&text={}";
        hoogle = "https://hoogle.haskell.org/?hoogle={}";
        jisho = "https://jisho.org/search/{}";
        kotobank = "https://kotobank.jp/gs/?q={}";
        kubernetes = "https://kubernetes.io/search/?q={}";
        lastfm = "https://www.last.fm/search?q={}";
        lobsters = "https://lobste.rs/search?q=test{}";
        mdn = "https://developer.mozilla.org/en-US/search?q={}";
        melpa = "https://melpa.org/#/?q={}";
        moddb = "https://www.moddb.com/search?q={}";
        musicbrainz = "https://musicbrainz.org/search?query={}";
        nix-issues = "https://github.com/NixOS/nix/issues?q={}";
        nix-prs = "https://github.com/NixOS/nix/pulls?q={}";
        nixos-flakes = "https://search.nixos.org/flakes?query={}";
        nixos-options = "https://search.nixos.org/options?query={}";
        nixos-packages = "https://search.nixos.org/packages?query={}";
        nixos-wiki = "https://nixos.wiki/index.php?search={}";
        nixpkgs-issues = "https://github.com/NixOS/nixpkgs/issues?q={}";
        nixpkgs-prs = "https://github.com/NixOS/nixpkgs/pulls?q={}";
        openstreetmap = "https://www.openstreetmap.org/search?query={}";
        ozon = "https://www.ozon.ru/search/?text={}";
        protondb = "https://www.protondb.com/search?q={}";
        pypi = "https://pypi.org/search/?q={}";
        pythondocs = "https://docs.python.org/3/search.html?q={}";
        rateyourmusic = "https://rateyourmusic.com/search?searchterm={}";
        riichi = "https://riichi.wiki/index.php?search={}";
        rustdoc = "https://doc.rust-lang.org/std/?search={}";
        searx = "https://searx.tiekoetter.com/search?q={}";
        slashdot = "https://slashdot.org/index2.pl?fhfilter={}";
        sourcehut = "https://sr.ht/projects?search={}";
        steam = "https://store.steampowered.com/search/?term={}";
        steamdb = "https://steamdb.info/search/?a=app&q={}";
        ubuntu = "https://wiki.ubuntu.com/Home?action=fullsearch&value={}";
        wikipedia-en = "https://en.wikipedia.org/w/index.php?search={}";
        wikipedia-ru = "https://ru.wikipedia.org/w/index.php?search={}";
        wikipedia-ja = "https://ja.wikipedia.org/w/index.php?search={}";
        wolphramalpha = "https://www.wolframalpha.com/input/?i={}";
        yahoo = "https://yahoo.com/search/?text={}";
        yahoo-images = "https://yahoo.com/images/search?text={}";
        yahoo-market = "https://market.yahoo.com/search?text={}";
        youtube = "https://yewtu.be/search?q={}";

        aw = arch;
        d = duckduckgo;
        do = dockerhub;
        docker = dockerhub;
        g = google;
        gh = github;
        h = hoogle;
        k = kubernetes;
        mb = musicbrainz;
        n = nixos-options;
        nw = nixos-wiki;
        py = pypi;
        pyd = pythondocs;
        rym = rateyourmusic;
        s = searx;
        sh = sourcehut;
        sr = sourcehut;
        w = wikipedia-en;
        wen = wikipedia-en;
        wru = wikipedia-ru;
        y = yahoo;
        yt = youtube;
      };

      settings = {
        changelog_after_upgrade = "never";

        content = {
          autoplay = false;
          cookies.accept = "all";
          default_encoding = "utf-8";
          desktop_capture = "ask";
          dns_prefetch = false;
          geolocation = false;
          headers.do_not_track = true;
          javascript.enabled = true;
          prefers_reduced_motion = true;
          webgl = true;

          blocking = {
            enabled = true;
            method = "adblock";
            adblock.lists = [
              "https://easylist.to/easylist/easylist.txt"
              "https://easylist.to/easylist/easyprivacy.txt"
              "https://easylist.to/easylist/fanboy-social.txt"
              "https://secure.fanboy.co.nz/fanboy-annoyance.txt"
              "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt"
            ];
          };
        };

        completion = {
          height = "50%";
          show = "auto";
          shrink = true;
          timestamp_format = "%y-%m-%d";
          min_chars = 3;
          open_categories = [
            "bookmarks"
            "quickmarks"
            "history"
          ];

          scrollbar = {
            width = 0;
            padding = 0;
          };
        };

        hints = {
          auto_follow = "unique-match";
          auto_follow_timeout = 0;
          border = "0px";
          min_chars = 1;
          scatter = false;
          uppercase = false;
        };

        hints.radius = 0;
        keyhint.radius = 0;
        prompt.radius = 0;

        spellcheck.languages = cfg.dicts;

        statusbar.position = "bottom";

        tabs = {
          position = "top";

          title = {
            alignment = "left";
            format = "{audio}{index} : {current_title}";
            format_pinned = "{audio}{index}";
          };

          min_width = -1;
          max_width = -1;

          indicator.width = 0;

          pinned = {
            shrink = true;
            frozen = false;
          };

          close_mouse_button = "middle";
          mousewheel_switching = false;

          background = true;
          select_on_remove = "next";
          new_position = {
            related = "next";
            unrelated = "last";
          };

          favicons = {
            show = "pinned";
            scale = 0.75;
          };
        };

        url = rec {
          default_page = "about:blank";
          start_pages = [ default_page ];
        };

        window = {
          hide_decoration = false;
          title_format = "{perc}{current_title}{title_sep}qutebrowser";
        };

        colors = with config.modules.themes.colorScheme; {
          completion = rec {
            fg = white;
            match.fg = red;
            odd.bg = black;
            even.bg = odd.bg;
            category = {
              fg = white;
              bg = black;
              border = {
                top = black;
                bottom = black;
              };
            };
            item.selected = {
              fg = black;
              bg = white;
              border = {
                top = white;
                bottom = white;
              };
            };
            scrollbar = {
              fg = white;
              bg = black;
            };
          };
          contextmenu = {
            menu = {
              fg = white;
              bg = black;
            };
            selected = {
              fg = black;
              bg = white;
            };
            disabled = {
              fg = brightBlack;
              bg = black;
            };
          };
          downloads = {
            bar.bg = black;
            start = {
              fg = green;
              bg = black;
            };
            stop = {
              fg = yellow;
              bg = black;
            };
            error = {
              fg = red;
              bg = black;
            };
            system = {
              fg = "none";
              bg = "none";
            };
          };
          hints = {
            fg = white;
            match.fg = red;
            bg = black;
          };
          keyhint = {
            fg = white;
            suffix.fg = red;
            bg = black;
          };
          messages = {
            error = rec {
              bg = black;
              fg = red;
              border = bg;
            };
            info = rec {
              fg = blue;
              bg = black;
              border = bg;
            };
            warning = rec {
              fg = yellow;
              bg = black;
              border = bg;
            };
          };
          prompts = rec {
            fg = white;
            bg = black;
            selected = {
              fg = black;
              bg = white;
            };
            border = bg;
          };
          statusbar = {
            normal = {
              bg = black;
              fg = white;
            };
            command = {
              bg = black;
              fg = white;
            };
            insert = {
              bg = green;
              fg = black;
            };
            passthrough = {
              bg = blue;
              fg = black;
            };
            private = {
              bg = magenta;
              fg = black;
            };
            url = {
              fg = blue;
              hover.fg = brightBlue;
              success = {
                http.fg = brightGreen;
                https.fg = brightGreen;
              };
              warn.fg = brightYellow;
              error.fg = brightRed;
            };
          };
          tabs = rec {
            bar.bg = black;
            even = {
              bg = black;
              fg = white;
            };
            odd = with even; {
              inherit bg fg;
            };
            selected = rec {
              even = {
                bg = white;
                fg = black;
              };
              odd = with even; {
                inherit bg fg;
              };
            };
            pinned = rec {
              even = {
                bg = brightBlack;
                fg = brightWhite;
              };
              odd = with even; {
                inherit bg fg;
              };
            };
            indicator = {
              start = green;
              stop = yellow;
              error = red;
              system = "none";
            };
          };
          webpage = {
            preferred_color_scheme = "dark";
          };
        };

        fonts = {
          default_size = (toString 14) + "pt";
        };
      };

      extraConfig =
        let
          mkPaddingDictionary =
            {
              name,
              bottom,
              left,
              right,
              top,
            }:
            let
              n = "c.${name}.padding";
              b = "'bottom': ${toString bottom}";
              l = "'left': ${toString left}";
              r = "'right': ${toString right}";
              t = "'top': ${toString top}";
            in
            "${n} = {${b}, ${l}, ${r}, ${t}}";

          final = map mkPaddingDictionary [
            {
              name = "hints";
              bottom = 3;
              left = 3;
              right = 3;
              top = 3;
            }
            {
              name = "statusbar";
              bottom = 1;
              left = 0;
              right = 3;
              top = 1;
            }
            {
              name = "tabs";
              bottom = 1;
              left = 6;
              right = 6;
              top = 1;
            }
          ];
        in
        concatLines final + "\n";
    };

    # Install language dictionaries for spellcheck backends
    system.userActivationScripts.qutebrowserInstallDicts =
      concatStringsSep
        ''
          \
        ''
        (
          map (lang: ''
            if ! find "$XDG_DATA_HOME/qutebrowser/qtwebengine_dictionaries" -name "${lang}*" 2>/dev/null | grep -q .; then
              ${getExe pkgs.python3} ${pkg}/share/qutebrowser/scripts/dictcli.py install ${lang}
            fi
          '') cfg.dicts
        );
  };
}
