let
  updateOnceInAWeek = 7 * 24 * 60 * 60 * 1000;
in
{
  "Amazon.com".metaData.hidden = true;
  "Bing".metaData.hidden = true;
  "Ebay".metaData.hidden = true;

  "AliExpress" = {
    urls = [ { template = "https://aliexpress.com/wholesale?SearchText={searchTerms}"; } ];
    iconUpdateURL = "https://ae01.alicdn.com/images/eng/wholesale/icon/aliexpress.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@aliexpress"
      "@ali"
    ];
  };

  "Docker Hub" = {
    urls = [ { template = "https://hub.docker.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.docker.com/wp-content/uploads/2023/04/cropped-Docker-favicon-32x32.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@dockerhub"
      "@docker"
    ];
  };

  "GitHub" = {
    urls = [ { template = "https://github.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://github.githubassets.com/favicons/favicon-dark.svg";
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
    iconUpdateURL = "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg";
    updateInterval = updateOnceInAWeek;
  };
  "Github Repos" = {
    urls = [ { template = "https://github.com/search?q={searchTerms}&type=repositories"; } ];
    definedAliases = [
      "@gh"
      "@repo"
    ];
    iconUpdateURL = "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg";
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
    iconUpdateURL = "https://gitlab.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@gl" ];
  };

  "SourceHut" = {
    urls = [ { template = "https://sr.ht/projects?search={searchTerms}"; } ];
    iconUpdateURL = "https://sr.ht/static/logo.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@sourcehut"
      "@srht"
    ];
  };

  "Genius" = {
    urls = [ { template = "https://genius.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://assets.genius.com/images/apple-touch-icon.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@genius" ];
  };

  "godocs.io" = {
    urls = [ { template = "https://godocs.io/?q={searchTerms}"; } ];
    iconUpdateURL = "https://go.dev/images/favicon-gopher.svg";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@godocs" ];
  };
  "pkgs.go.dev" = {
    urls = [ { template = "https://pkg.go.dev/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://go.dev/images/favicon-gopher.svg";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@gopkgs" ];
  };

  "Hackage" = {
    urls = [ { template = "https://hackage.haskell.org/packages/search?terms={searchTerms}"; } ];
    iconUpdateURL = "https://hackage.haskell.org/static/favicon.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@hackage" ];
  };
  "Hoogle" = {
    urls = [ { template = "https://hoogle.haskell.org/?hoogle={searchTerms}"; } ];
    iconUpdateURL = "https://hoogle.haskell.org/favicon.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@hoogle" ];
  };
  "Flora" = {
    urls = [ { template = "https://flora.pm/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://flora.pm/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@flora" ];
  };

  "Kubernetes" = {
    urls = [ { template = "https://kubernetes.io/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://kubernetes.io/images/kubernetes.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@kubernetes"
      "@k8s"
    ];
  };

  "Last.fm" = {
    urls = [ { template = "https://www.last.fm/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.last.fm/static/images/favicon.702b239b6194.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@lastfm" ];
  };

  "MDN" = {
    urls = [ { template = "https://developer.mozilla.org/en-US/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://developer.mozilla.org/favicon-48x48.cbbd161b.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@mdn" ];
  };

  "MELPA" = {
    urls = [ { template = "https://melpa.org/#/?q={searchTerms}"; } ];
    iconUpdateURL = "https://melpa.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@melpa" ];
  };

  "NixOS Packages" = {
    urls = [ { template = "https://search.nixos.org/packages?channel=unstable&query={searchTerms}"; } ];
    iconUpdateURL = "https://nixos.org/favicon.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixpkgs"
      "@np"
    ];
  };

  "NixOS Options" = {
    urls = [ { template = "https://search.nixos.org/options?channel=unstable&query={searchTerms}"; } ];
    iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixopts"
      "@no"
    ];
  };

  "NixOS Wiki" = {
    urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];
    iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nixosw" ];
  };

  "NixOS Issues" = {
    urls = [
      { template = "https://github.com/NixOS/nixpkgs/issues?q=is%3Aissue+is%3Aopen+{searchTerms}"; }
    ];
    iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixissues"
    ];
  };

  "Nixpkgs PRs" = {
    urls = [ { template = "https://github.com/NixOS/nixpkgs/pulls?q={searchTerms}"; } ];
    iconUpdateURL = "https://nixos.org/favicon.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nixpr" ];
  };

  "noogle" = {
    urls = [ { template = "https://noogle.dev/q?term={searchTerms}"; } ];
    iconUpdateURL = "https://noogle.dev/favicon.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@noogle" ];
  };

  "Home manager options" = {
    urls = [ { template = "https://home-manager-options.extranix.com/?query={searchTerms}"; } ];
    iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@hmopts"
      "@hm"
    ];
  };

  "Mynixos" = {
    urls = [ { template = "https://mynixos.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://mynixos.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@mynixos" ];
  };

  "Nix Reference Manual" = {
    urls = [ { template = "https://nixos.org/manual/nix/unstable/?search={searchTerms}"; } ];
    iconUpdateURL = "https://nixos.org/manual/nix/unstable/favicon.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nm"
      "@nixman"
      "@nixmanual"
    ];
  };

  "PyPI" = {
    urls = [ { template = "https://pypi.org/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://pypi.org/static/images/logo-small.2a411bc6.svg";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pypi" ];
  };

  "Python Docs" = {
    urls = [ { template = "https://docs.python.org/3/search.html?q={searchTerms}"; } ];
    iconUpdateURL = "https://docs.python.org/3/_static/py.svg";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pydocs" ];
  };

  "crates.io" = {
    urls = [ { template = "https://crates.io/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://crates.io/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@crates" ];
  };
  "Rust Std" = {
    urls = [ { template = "https://doc.rust-lang.org/std/?search={searchTerms}"; } ];
    iconUpdateURL = "https://www.rust-lang.org/static/images/favicon-32x32.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@ruststd"
      "@rust"
    ];
  };
  "Rust Language Documentation" = {
    urls = [ { template = "https://doc.rust-lang.org/std/?search={searchTerms}"; } ];
    iconUpdateURL = "https://doc.rust-lang.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@r"
      "@rust"
    ];
  };
  "Rust Crates Documentation" = {
    urls = [ { template = "https://docs.rs/releases/search?query={searchTerms}"; } ];
    iconUpdateURL = "https://docs.rs/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@drs"
      "@docsrs"
    ];
  };
  "Lib.rs" = {
    urls = [ { template = "https://lib.rs/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://lib.rs/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@lrs"
      "@librs"
    ];
  };

  "WolframAlpha" = {
    urls = [ { template = "https://www.wolframalpha.com/input?i={searchTerms}"; } ];
    iconUpdateURL = "https://www.wolframalpha.com/_next/static/images/favicon_1zbE9hjk.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@wolframalpha"
      "@wa"
    ];
  };
  "MathWorld" = {
    urls = [ { template = "https://mathworld.wolfram.com/search/?query={searchTerms}"; } ];
    iconUpdateURL = "https://mathworld.wolfram.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@mathworld" ];
  };

  "Stack Overflow" = {
    urls = [ { template = "https://stackoverflow.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://cdn.sstatic.net/Sites/stackoverflow/Img/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@stackoverflow"
      "@so"
    ];
  };

  "npm" = {
    urls = [ { template = "https://www.npmjs.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://static.npmjs.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@npm" ];
  };

  "Reddit" = {
    urls = [ { template = "https://www.reddit.com/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.reddit.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@reddit"
      "@r"
    ];
  };

  "YouTube" = {
    urls = [ { template = "https://www.youtube.com/results?search_query={searchTerms}"; } ];
    iconUpdateURL = "https://www.youtube.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@youtube"
      "@yt"
    ];
  };

  "Wikipedia" = {
    urls = [ { template = "https://en.wikipedia.org/w/index.php?search={searchTerms}"; } ];
    iconUpdateURL = "https://en.wikipedia.org/static/favicon/wikipedia.ico";
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
    iconUpdateURL = "https://en.wiktionary.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@wiktionary" ];
  };

  "Amazon" = {
    urls = [ { template = "https://www.amazon.com/s?k={searchTerms}"; } ];
    iconUpdateURL = "https://www.amazon.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@amazon" ];
  };

  "DuckDuckGo" = {
    urls = [ { template = "https://duckduckgo.com/?q={searchTerms}"; } ];
    iconUpdateURL = "https://duckduckgo.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@duckduckgo"
      "@ddg"
    ];
  };

  "eBay" = {
    urls = [ { template = "https://www.ebay.com/sch/i.html?_nkw={searchTerms}"; } ];
    iconUpdateURL = "https://www.ebay.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@ebay"
      "@eb"
    ];
  };

  "Twitter" = {
    urls = [ { template = "https://twitter.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://twitter.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@twitter"
      "@x"
      "@tw"
    ];
  };

  "olx.ua" = {
    urls = [ { template = "https://www.olx.ua/uk/list/q-{searchTerms}/"; } ];
    iconUpdateURL = "https://www.olx.ua/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@olx" ];
  };

  "Google Fonts" = {
    urls = [ { template = "https://fonts.google.com/?query={searchTerms}"; } ];
    iconUpdateURL = "https://www.gstatic.com/images/icons/material/apps/fonts/1x/catalog/v5/favicon.svg";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@goofonts" ];
  };

  "DevDocs" = {
    urls = [ { template = "https://devdocs.io/#q={searchTerms}"; } ];
    iconUpdateURL = "https://devdocs.io/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@devdocs" ];
  };

  "Can I Use" = {
    urls = [ { template = "https://caniuse.com/?search={searchTerms}"; } ];
    iconUpdateURL = "https://caniuse.com/img/favicon-128.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@caniuse" ];
  };

  "Hugging Face" = {
    urls = [ { template = "https://huggingface.co/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://huggingface.co/favicon.ico";
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
    iconUpdateURL = "https://huggingface.co/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@hfmodels"
      "@huggingfacemodels"
    ];
  };

  "Papers With Code" = {
    urls = [ { template = "https://paperswithcode.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://paperswithcode.com/favicon-32x32.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@pwc"
      "@paperswithcode"
    ];
  };

  Perplexity = {
    urls = [ { template = "https://www.perplexity.ai/?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.perplexity.ai/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@p"
      "@perplexity"
    ];
  };

  "Phind" = {
    urls = [ { template = "https://www.phind.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.phind.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@phind" ];
  };

  "Qwant" = {
    urls = [ { template = "https://www.qwant.com/?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.qwant.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@qwant" ];
  };

  "Qwant Images" = {
    urls = [ { template = "https://www.qwant.com/?t=images&q={searchTerms}"; } ];
    iconUpdateURL = "https://upload.wikimedia.org/wikipedia/commons/2/2b/Qwant-Icone-2022.svg";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@qwantimg"
      "@qimg"
    ];
  };

  "Flathub" = {
    urls = [ { template = "https://flathub.org/apps/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://styles.redditmedia.com/t5_3k6jw/styles/communityIcon_b1hyv6wssjd91.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@flathub"
      "@flatpak"
    ];
  };

  "Arch Wiki" = {
    urls = [ { template = "https://wiki.archlinux.org/index.php?search={searchTerms}"; } ];
    iconUpdateURL = "https://wiki.archlinux.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@alwiki" ];
  };

  "SearXNG" = {
    urls = [ { template = "https://search.inetol.net/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://search.inetol.net/favicon.ico";
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
    iconUpdateURL = "https://brave.com/static-assets/images/brave-logo-sans-text.svg";
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
    iconUpdateURL = "https://brave.com/static-assets/images/brave-logo-sans-text.svg";
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
    iconUpdateURL = "https://translate.google.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@gootr" ];
  };

  "Pinterest" = {
    urls = [ { template = "https://www.pinterest.com/search/pins/?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.pinterest.com/favicon.ico";
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
    iconUpdateURL = "https://hn.algolia.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@hn"
      "@hackernews"
    ];
  };

  "fmhy" = {
    urls = [ { template = "https://fmhy.pages.dev/?q={searchTerms}"; } ];
    iconUpdateURL = "https://fmhy.pages.dev/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@fmhy"
      "@freethings"
    ];
  };

  "Repology - Options Search" = {
    urls = [ { template = "https://repology.org/project/{searchTerms}/versions"; } ];
    iconUpdateURL = "https://repology.org/favicon.ico";
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
    iconUpdateURL = "https://www.twitch.tv/favicon.ico";
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
    iconUpdateURL = "https://www.tradingview.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@tv"
      "@tradingview"
    ];
  };
  "Dexscreener" = {
    urls = [ { template = "https://dexscreener.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://dexscreener.com/favicon.ico";
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
    iconUpdateURL = "https://www.urbandictionary.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@urban" ];
  };
  "Dictionary" = {
    urls = [ { template = "https://dictionary.reference.com/browse/{searchTerms}"; } ];
    iconUpdateURL = "https://dictionary.reference.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@dict" ];
  };
  "Thesaurus" = {
    urls = [ { template = "https://thesaurus.com/browse/{searchTerms}"; } ];
    iconUpdateURL = "https://thesaurus.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@thesaurus" ];
  };
  "Vocabulary" = {
    urls = [ { template = "https://www.vocabulary.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.vocabulary.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@vocabulary" ];
  };
  "Forvo" = {
    urls = [ { template = "https://forvo.com/search/{searchTerms}/"; } ];
    iconUpdateURL = "https://forvo.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pronounce" ];
  };

  "IMDB" = {
    urls = [ { template = "https://www.imdb.com/find?s=all&q={searchTerms}"; } ];
    iconUpdateURL = "https://www.imdb.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@imdb" ];
  };
  "Letterboxd" = {
    urls = [ { template = "https://letterboxd.com/search/{searchTerms}/"; } ];
    iconUpdateURL = "https://letterboxd.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@letterboxd" ];
  };

  "ProtonDB" = {
    urls = [ { template = "https://www.protondb.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.protondb.com/sites/protondb/images/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@protondb" ];
  };
  "SteamDB" = {
    urls = [ { template = "https://steamdb.info/search/?a=app&q={searchTerms}"; } ];
    iconUpdateURL = "https://steamdb.info/static/logos/32px.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@steamdb" ];
  };

  "Debian" = {
    urls = [ { template = "https://packages.debian.org/search?keywords={searchTerms}"; } ];
    iconUpdateURL = "https://packages.debian.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@deb" ];
  };
  "Ubuntu" = {
    urls = [ { template = "https://packages.ubuntu.com/search?keywords={searchTerms}"; } ];
    iconUpdateURL = "https://packages.ubuntu.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@ubuntu" ];
  };

  "Maps" = {
    urls = [ { template = "https://maps.google.com/maps?q={searchTerms}"; } ];
    iconUpdateURL = "https://maps.google.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@maps"
      "@gmaps"
    ];
  };
  "OpenStreetMap" = {
    urls = [ { template = "https://www.openstreetmap.org/search?query={searchTerms}"; } ];
    iconUpdateURL = "https://www.openstreetmap.org/assets/favicon-32x32-99b88fcadeef736889823c8a886b89d8cada9d4423a49a27de29bacc0a6bebd1.png";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@openstreetmap"
      "@osm"
      "@openmaps"
    ];
  };

  "Arch Packages" = {
    urls = [ { template = "https://archlinux.org/packages/?q={searchTerms}"; } ];
    iconUpdateURL = "https://archlinux.org/static/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@archpkg" ];
  };

  "Wayback Machine" = {
    urls = [ { template = "https://web.archive.org/web/*/{searchTerms}"; } ];
    iconUpdateURL = "https://web.archive.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@wayback" ];
  };

  "Metacritic" = {
    urls = [ { template = "https://www.metacritic.com/search/{searchTerms}/"; } ];
    iconUpdateURL = "https://www.metacritic.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@metacritic" ];
  };

  "Goodreads" = {
    urls = [ { template = "https://www.goodreads.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.goodreads.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@goodreads" ];
  };

  "Man Pages" = {
    urls = [ { template = "https://man.archlinux.org/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://man.archlinux.org/static/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@man" ];
  };

  "HTTP Status" = {
    urls = [ { template = "https://httpstatuses.com/{searchTerms}"; } ];
    iconUpdateURL = "https://httpstatuses.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@http"
      "@httpstatus"
    ];
  };

  "Emojipedia" = {
    urls = [ { template = "https://emojipedia.org/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://emojipedia.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@emoji" ];
  };

  "Invidious" = {
    urls = [ { template = "invidious.nerdvpn.de/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://invidious.nerdvpn.de/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@inv" ];
  };

  "AlternativeTo" = {
    urls = [ { template = "https://alternativeto.net/browse/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://alternativeto.net/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@alt"
      "alternative"
    ];
  };

  "Sourcegraph" = {
    urls = [ { template = "https://sourcegraph.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://sourcegraph.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@srcgraph" ];
  };

  "Awesome Lists" = {
    urls = [ { template = "https://github.com/search?q=awesome+{searchTerms}&type=repositories"; } ];
    iconUpdateURL = "https://github.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@awesome"
      "@ghawesome"
    ];
  };

  "Rosetta Code" = {
    urls = [ { template = "https://rosettacode.org/wiki/Special:Search?search={searchTerms}"; } ];
    iconUpdateURL = "https://rosettacode.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@rosetta" ];
  };

  "Nix Discourse" = {
    urls = [ { template = "https://discourse.nixos.org/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://discourse.nixos.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nixdis" ];
  };

  "Elixir Hex" = {
    urls = [ { template = "https://hex.pm/packages?search={searchTerms}&sort=recent_downloads"; } ];
    iconUpdateURL = "https://hex.pm/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@elixirhex"
      "@ehex"
    ];
  };

  "Unpsplash" = {
    urls = [ { template = "https://unsplash.com/s/photos/{searchTerms}"; } ];
    iconUpdateURL = "https://unsplash.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@unsplash" ];
  };
  "Artvee" = {
    urls = [ { template = "https://artvee.com/main/?s={searchTerms}"; } ];
    iconUpdateURL = "https://artvee.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@artvee" ];
  };

  "Flakehub" = {
    urls = [ { template = "https://flakehub.com/flakes?q={searchTerms}"; } ];
    iconUpdateURL = "https://flakehub.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@flakehub" ];
  };

  "ExplainShell" = {
    urls = [ { template = "https://explainshell.com/explain?cmd={searchTerms}"; } ];
    iconUpdateURL = "https://explainshell.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@explain" ];
  };

  "Cheat.sh" = {
    urls = [ { template = "https://cheat.sh/{searchTerms}"; } ];
    iconUpdateURL = "https://cheat.sh/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@cheatsh" ];
  };

  "arXiv" = {
    urls = [ { template = "https://arxiv.org/search/?query={searchTerms}&searchtype=all"; } ];
    iconUpdateURL = "https://arxiv.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@arxiv" ];
  };

  "Nixpkgs Tracker" = {
    urls = [ { template = "https://nixpkgs-tracker.ocfox.me/?pr={searchTerms}"; } ];
    iconUpdateURL = "https://ocfox.me/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@nixpkgstracker"
      "@nixtrack"
    ];
  };

  "Nuget" = {
    urls = [ { template = "https://www.nuget.org/packages?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.nuget.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@nuget" ];
  };

  "Libgen" = {
    urls = [
      {
        template = "https://www.libgen.is/search.php?req={searchTerms}&lg_topic=libgen&open=0&view=simple&res=25&phrase=1&column=def";
      }
    ];
    iconUpdateURL = "https://www.libgen.is/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@libgen" ];
  };

  "Know Your Meme" = {
    urls = [ { template = "https://knowyourmeme.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://knowyourmeme.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@meme" ];
  };

  "Recipe Search" = {
    urls = [ { template = "https://recipe-search.typesense.org/?q={searchTerms}"; } ];
    iconUpdateURL = "https://recipe-search.typesense.org/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@recipe" ];
  };

  "Whois Lookup" = {
    urls = [ { template = "https://who.is/whois/{searchTerms}"; } ];
    iconUpdateURL = "https://who.is/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@whois" ];
  };

  "Marginalia" = {
    urls = [ { template = "https://search.marginalia.nu/search?query={searchTerms}"; } ];
    iconUpdateURL = "https://search.marginalia.nu/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@old"
      "@marginalia"
    ];
  };

  "DNS Lookup" = {
    urls = [ { template = "https://dns.google/query?name={searchTerms}"; } ];
    iconUpdateURL = "https://dns.google/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@dns" ];
  };

  "Public APIs" = {
    urls = [ { template = "https://github.com/public-apis/public-apis/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://github.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@publicapi"
      "@pubapi"
    ];
  };

  "Downdetector" = {
    urls = [ { template = "https://downdetector.com/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://downdetector.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [
      "@downdetector"
    ];
  };

  "Pixabay" = {
    urls = [ { template = "https://pixabay.com/images/search/{searchTerms}"; } ];
    iconUpdateURL = "https://pixabay.com/favicon.ico";
    updateInterval = updateOnceInAWeek;
    definedAliases = [ "@pixabay" ];
  };
}
