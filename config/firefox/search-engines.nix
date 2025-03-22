let
  updateOnceInAWeek = 7 * 24 * 60 * 60 * 1000;
in
{
  "Amazon.com".metaData.hidden = true;
  "Bing".metaData.hidden = true;
  "Ebay".metaData.hidden = true;

  "AliExpress" = {
    urls = [ { template = "https://aliexpress.com/wholesale?SearchText={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@aliexpress"
      "@ali"
    ];
  };

  "Docker Hub" = {
    urls = [ { template = "https://hub.docker.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@dockerhub"
      "@docker"
    ];
  };

  "GitHub" = {
    urls = [ { template = "https://github.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@github"
      "@gh"
    ];
  };
  "Github Code" = {
    urls = [ { template = "https://github.com/search?q={searchTerms}&type=code"; } ];
    definedAliases = [
      "@code"
      "@ghc"
      "~gc"
    ];

    updateInterval = updateOnceInAWeek;
  };
  "Github Repos" = {
    urls = [ { template = "https://github.com/search?q={searchTerms}&type=repositories"; } ];
    definedAliases = [
      "@gh"
      "@repo"
    ];

    updateInterval = updateOnceInAWeek;
  };

  "GitLab" = {
    urls = [
      {
        template = "https://gitlab.com/search";
        params = [
          {
            name = "search";
            value = "{searchTerms}";
          }
        ];
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@gl" ];
  };

  "SourceHut" = {
    urls = [ { template = "https://sr.ht/projects?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@sourcehut"
      "@srht"
    ];
  };

  "Genius" = {
    urls = [ { template = "https://genius.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@genius" ];
  };

  "godocs.io" = {
    urls = [ { template = "https://godocs.io/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@godocs" ];
  };
  "pkgs.go.dev" = {
    urls = [ { template = "https://pkg.go.dev/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@gopkgs" ];
  };

  "Hackage" = {
    urls = [ { template = "https://hackage.haskell.org/packages/search?terms={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@hackage" ];
  };
  "Hoogle" = {
    urls = [ { template = "https://hoogle.haskell.org/?hoogle={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@hoogle" ];
  };
  "Flora" = {
    urls = [ { template = "https://flora.pm/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@flora" ];
  };

  "Kubernetes" = {
    urls = [ { template = "https://kubernetes.io/search/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@kubernetes"
      "@k8s"
    ];
  };

  "Last.fm" = {
    urls = [ { template = "https://www.last.fm/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@lastfm" ];
  };

  "MDN" = {
    urls = [ { template = "https://developer.mozilla.org/en-US/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@mdn" ];
  };

  "MELPA" = {
    urls = [ { template = "https://melpa.org/#/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@melpa" ];
  };

  "NixOS Packages" = {
    urls = [ { template = "https://search.nixos.org/packages?channel=unstable&query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixpkgs"
      "@np"
    ];
  };

  "NixOS Options" = {
    urls = [ { template = "https://search.nixos.org/options?channel=unstable&query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixopts"
      "@no"
    ];
  };

  "NixOS Wiki" = {
    urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nixosw" ];
  };

  "NixOS Issues" = {
    urls = [
      { template = "https://github.com/NixOS/nixpkgs/issues?q=is%3Aissue+is%3Aopen+{searchTerms}"; }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixissues"
    ];
  };

  "Nixpkgs PRs" = {
    urls = [ { template = "https://github.com/NixOS/nixpkgs/pulls?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nixpr" ];
  };

  "noogle" = {
    urls = [ { template = "https://noogle.dev/q?term={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@noogle" ];
  };

  "Home manager options" = {
    urls = [ { template = "https://home-manager-options.extranix.com/?query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@hmopts"
      "@hm"
    ];
  };

  "Mynixos" = {
    urls = [ { template = "https://mynixos.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@mynixos" ];
  };

  "Nix Reference Manual" = {
    urls = [ { template = "https://nixos.org/manual/nix/unstable/?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nm"
      "@nixman"
      "@nixmanual"
    ];
  };

  "PyPI" = {
    urls = [ { template = "https://pypi.org/search/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pypi" ];
  };

  "Python Docs" = {
    urls = [ { template = "https://docs.python.org/3/search.html?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pydocs" ];
  };

  "crates.io" = {
    urls = [ { template = "https://crates.io/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@crates" ];
  };
  "Rust Std" = {
    urls = [ { template = "https://doc.rust-lang.org/std/?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@ruststd"
      "@rust"
    ];
  };
  "Rust Language Documentation" = {
    urls = [ { template = "https://doc.rust-lang.org/std/?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@r"
      "@rust"
    ];
  };
  "Rust Crates Documentation" = {
    urls = [ { template = "https://docs.rs/releases/search?query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@drs"
      "@docsrs"
    ];
  };
  "Lib.rs" = {
    urls = [ { template = "https://lib.rs/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@lrs"
      "@librs"
    ];
  };

  "WolframAlpha" = {
    urls = [ { template = "https://www.wolframalpha.com/input?i={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@wolframalpha"
      "@wa"
    ];
  };
  "MathWorld" = {
    urls = [ { template = "https://mathworld.wolfram.com/search/?query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@mathworld" ];
  };

  "Stack Overflow" = {
    urls = [ { template = "https://stackoverflow.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@stackoverflow"
      "@so"
    ];
  };

  "npm" = {
    urls = [ { template = "https://www.npmjs.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@npm" ];
  };

  "Reddit" = {
    urls = [ { template = "https://www.reddit.com/search/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@reddit"
      "@r"
    ];
  };

  "YouTube" = {
    urls = [ { template = "https://www.youtube.com/results?search_query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@youtube"
      "@yt"
    ];
  };

  "Wikipedia" = {
    urls = [ { template = "https://en.wikipedia.org/w/index.php?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@wikipedia"
      "@wiki"
    ];
  };
  "Wiktionary" = {
    urls = [
      { template = "https://en.wiktionary.org/wiki/Special:Search?go=Go&search={searchTerms}"; }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@wiktionary" ];
  };

  "Amazon" = {
    urls = [ { template = "https://www.amazon.com/s?k={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@amazon" ];
  };

  "DuckDuckGo" = {
    urls = [ { template = "https://duckduckgo.com/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@duckduckgo"
      "@ddg"
    ];
  };

  "eBay" = {
    urls = [ { template = "https://www.ebay.com/sch/i.html?_nkw={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@ebay"
      "@eb"
    ];
  };

  "Twitter" = {
    urls = [ { template = "https://twitter.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@twitter"
      "@x"
      "@tw"
    ];
  };

  "olx.ua" = {
    urls = [ { template = "https://www.olx.ua/uk/list/q-{searchTerms}/"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@olx" ];
  };

  "Google Fonts" = {
    urls = [ { template = "https://fonts.google.com/?query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@goofonts" ];
  };

  "DevDocs" = {
    urls = [ { template = "https://devdocs.io/#q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@devdocs" ];
  };

  "Can I Use" = {
    urls = [ { template = "https://caniuse.com/?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@caniuse" ];
  };

  "Hugging Face" = {
    urls = [ { template = "https://huggingface.co/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@hf"
      "@huggingface"
    ];
  };

  "Hugging Face Models" = {
    urls = [
      {
        template = "https://huggingface.co/models";
        params = [
          {
            name = "search";
            value = "{searchTerms}";
          }
        ];
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@hfmodels"
      "@huggingfacemodels"
    ];
  };

  "Papers With Code" = {
    urls = [ { template = "https://paperswithcode.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@pwc"
      "@paperswithcode"
    ];
  };

  Perplexity = {
    urls = [ { template = "https://www.perplexity.ai/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@p"
      "@perplexity"
    ];
  };

  "Phind" = {
    urls = [ { template = "https://www.phind.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@phind" ];
  };

  "Qwant" = {
    urls = [ { template = "https://www.qwant.com/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@qwant" ];
  };

  "Qwant Images" = {
    urls = [ { template = "https://www.qwant.com/?t=images&q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@qwantimg"
      "@qimg"
    ];
  };

  "Flathub" = {
    urls = [ { template = "https://flathub.org/apps/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@flathub"
      "@flatpak"
    ];
  };

  "Arch Wiki" = {
    urls = [ { template = "https://wiki.archlinux.org/index.php?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@alwiki" ];
  };

  "SearXNG" = {
    urls = [ { template = "https://search.inetol.net/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@sr"
      "@searx"
    ];
  };

  "Brave" = {
    urls = [
      {
        template = "https://search.brave.com/search";
        params = [
          {
            name = "q";
            value = "{searchTerms}";
          }
        ];
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@bb"
      "@brave"
    ];
  };
  "Brave Summary" = {
    urls = [
      {
        template = "https://search.brave.com/search";
        params = [
          {
            name = "q";
            value = "{searchTerms}";
          }
          {
            name = "summary";
            value = "1";
          }
        ];
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@bs"
      "@bravesum"
    ];
  };

  "Translate" = {
    urls = [
      {
        template = "https://translate.google.com/?#auto|auto|{searchTerms}";
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@gootr" ];
  };

  "Pinterest" = {
    urls = [ { template = "https://www.pinterest.com/search/pins/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@pinterest"
      "@pin"
    ];
  };

  "HackerNews" = {
    urls = [
      {
        template = "https://hn.algolia.com/";
        params = [
          {
            name = "query";
            value = "{searchTerms}";
          }
        ];
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@hn"
      "@hackernews"
    ];
  };

  "fmhy" = {
    urls = [ { template = "https://fmhy.pages.dev/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@fmhy"
      "@freethings"
    ];
  };

  "Repology - Options Search" = {
    urls = [ { template = "https://repology.org/project/{searchTerms}/versions"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@repology"
      "@repo"
    ];
  };

  "Twitch" = {
    urls = [
      {
        template = "https://www.twitch.tv/{searchTerms}";
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@twitch" ];
  };

  "TradingView.com" = {
    urls = [
      {
        template = "https://www.tradingview.com/chart/";
        params = [
          {
            name = "symbol";
            value = "{searchTerms}";
          }
        ];
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@tv"
      "@tradingview"
    ];
  };
  "Dexscreener" = {
    urls = [ { template = "https://dexscreener.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@dex" ];
  };

  "Urban Dictionary" = {
    urls = [
      {
        template = "https://www.urbandictionary.com/define.php";
        params = [
          {
            name = "term";
            value = "{searchTerms}";
          }
        ];
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@urban" ];
  };
  "Dictionary" = {
    urls = [ { template = "https://dictionary.reference.com/browse/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@dict" ];
  };
  "Thesaurus" = {
    urls = [ { template = "https://thesaurus.com/browse/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@thesaurus" ];
  };
  "Vocabulary" = {
    urls = [ { template = "https://www.vocabulary.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@vocabulary" ];
  };
  "Forvo" = {
    urls = [ { template = "https://forvo.com/search/{searchTerms}/"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pronounce" ];
  };

  "IMDB" = {
    urls = [ { template = "https://www.imdb.com/find?s=all&q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@imdb" ];
  };
  "Letterboxd" = {
    urls = [ { template = "https://letterboxd.com/search/{searchTerms}/"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@letterboxd" ];
  };

  "ProtonDB" = {
    urls = [ { template = "https://www.protondb.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@protondb" ];
  };
  "SteamDB" = {
    urls = [ { template = "https://steamdb.info/search/?a=app&q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@steamdb" ];
  };

  "Debian" = {
    urls = [ { template = "https://packages.debian.org/search?keywords={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@deb" ];
  };
  "Ubuntu" = {
    urls = [ { template = "https://packages.ubuntu.com/search?keywords={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@ubuntu" ];
  };

  "Maps" = {
    urls = [ { template = "https://maps.google.com/maps?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@maps"
      "@gmaps"
    ];
  };
  "OpenStreetMap" = {
    urls = [ { template = "https://www.openstreetmap.org/search?query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@openstreetmap"
      "@osm"
      "@openmaps"
    ];
  };

  "Arch Packages" = {
    urls = [ { template = "https://archlinux.org/packages/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@archpkg" ];
  };

  "Wayback Machine" = {
    urls = [ { template = "https://web.archive.org/web/*/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@wayback" ];
  };

  "Metacritic" = {
    urls = [ { template = "https://www.metacritic.com/search/{searchTerms}/"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@metacritic" ];
  };

  "Goodreads" = {
    urls = [ { template = "https://www.goodreads.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@goodreads" ];
  };

  "Man Pages" = {
    urls = [ { template = "https://man.archlinux.org/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@man" ];
  };

  "HTTP Status" = {
    urls = [ { template = "https://httpstatuses.com/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@http"
      "@httpstatus"
    ];
  };

  "Emojipedia" = {
    urls = [ { template = "https://emojipedia.org/search/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@emoji" ];
  };

  "Invidious" = {
    urls = [ { template = "invidious.nerdvpn.de/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@inv"
      "@invidious"
    ];
  };

  "AlternativeTo" = {
    urls = [ { template = "https://alternativeto.net/browse/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@alt"
      "alternative"
    ];
  };

  "Sourcegraph" = {
    urls = [ { template = "https://sourcegraph.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@srcgraph" ];
  };

  "Awesome Lists" = {
    urls = [ { template = "https://github.com/search?q=awesome+{searchTerms}&type=repositories"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@awesome"
      "@ghawesome"
    ];
  };

  "Rosetta Code" = {
    urls = [ { template = "https://rosettacode.org/wiki/Special:Search?search={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@rosetta" ];
  };

  "Nix Discourse" = {
    urls = [ { template = "https://discourse.nixos.org/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nixdis" ];
  };

  "Elixir Hex" = {
    urls = [ { template = "https://hex.pm/packages?search={searchTerms}&sort=recent_downloads"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@elixirhex"
      "@ehex"
    ];
  };

  "Unpsplash" = {
    urls = [ { template = "https://unsplash.com/s/photos/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@unsplash" ];
  };
  "Artvee" = {
    urls = [ { template = "https://artvee.com/main/?s={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@artvee" ];
  };

  "Flakehub" = {
    urls = [ { template = "https://flakehub.com/flakes?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@flakehub" ];
  };

  "ExplainShell" = {
    urls = [ { template = "https://explainshell.com/explain?cmd={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@explain" ];
  };

  "Cheat.sh" = {
    urls = [ { template = "https://cheat.sh/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@cheatsh" ];
  };

  "arXiv" = {
    urls = [ { template = "https://arxiv.org/search/?query={searchTerms}&searchtype=all"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@arxiv" ];
  };

  "Nixpkgs Tracker" = {
    urls = [ { template = "https://nixpkgs-tracker.ocfox.me/?pr={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixpkgstracker"
      "@nixtrack"
      "@nixprtracker"
    ];
  };

  "Nuget" = {
    urls = [ { template = "https://www.nuget.org/packages?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nuget" ];
  };

  "Libgen" = {
    urls = [
      {
        template = "https://www.libgen.is/search.php?req={searchTerms}&lg_topic=libgen&open=0&view=simple&res=25&phrase=1&column=def";
      }
    ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@libgen" ];
  };

  "Know Your Meme" = {
    urls = [ { template = "https://knowyourmeme.com/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@meme" ];
  };

  "Recipe Search" = {
    urls = [ { template = "https://recipe-search.typesense.org/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@recipe" ];
  };

  "Whois Lookup" = {
    urls = [ { template = "https://who.is/whois/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@whois" ];
  };

  "Marginalia" = {
    urls = [ { template = "https://search.marginalia.nu/search?query={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@old"
      "@marginalia"
    ];
  };

  "DNS Lookup" = {
    urls = [ { template = "https://dns.google/query?name={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@dns" ];
  };

  "Public APIs" = {
    urls = [ { template = "https://github.com/public-apis/public-apis/search?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@publicapi"
      "@pubapi"
    ];
  };

  "Downdetector" = {
    urls = [ { template = "https://downdetector.com/search/?q={searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@downdetector"
    ];
  };

  "Pixabay" = {
    urls = [ { template = "https://pixabay.com/images/search/{searchTerms}"; } ];

    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pixabay" ];
  };
}
