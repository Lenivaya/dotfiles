let
  updateEveryDay = 24 * 60 * 60 * 1000;
in
{
  "Amazon.com".metaData.hidden = true;
  "Bing".metaData.hidden = true;
  "Ebay".metaData.hidden = true;

  "AliExpress" = {
    urls = [ { template = "https://aliexpress.com/wholesale?SearchText={searchTerms}"; } ];
    iconUpdateURL = "https://ae01.alicdn.com/images/eng/wholesale/icon/aliexpress.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@aliexpress"
      "@ali"
    ];
  };

  "crates.io" = {
    urls = [ { template = "https://crates.io/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://crates.io/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@crates" ];
  };

  "Docker Hub" = {
    urls = [ { template = "https://hub.docker.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.docker.com/wp-content/uploads/2023/04/cropped-Docker-favicon-32x32.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@dockerhub"
      "@docker"
    ];
  };

  "GitHub" = {
    urls = [ { template = "https://github.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://github.githubassets.com/favicons/favicon-dark.svg";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@github"
      "@gh"
    ];
  };

  "Genius" = {
    urls = [ { template = "https://genius.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://assets.genius.com/images/apple-touch-icon.png";
    updateInterval = updateEveryDay;
    definedAliases = [ "@genius" ];
  };

  "godocs.io" = {
    urls = [ { template = "https://godocs.io/?q={searchTerms}"; } ];
    iconUpdateURL = "https://go.dev/images/favicon-gopher.svg";
    updateInterval = updateEveryDay;
    definedAliases = [ "@godocs" ];
  };

  "pkgs.go.dev" = {
    urls = [ { template = "https://pkg.go.dev/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://go.dev/images/favicon-gopher.svg";
    updateInterval = updateEveryDay;
    definedAliases = [ "@gopkgs" ];
  };

  "Hackage" = {
    urls = [ { template = "https://hackage.haskell.org/packages/search?terms={searchTerms}"; } ];
    iconUpdateURL = "https://hackage.haskell.org/static/favicon.png";
    updateInterval = updateEveryDay;
    definedAliases = [ "@hackage" ];
  };

  "Hoogle" = {
    urls = [ { template = "https://hoogle.haskell.org/?hoogle={searchTerms}"; } ];
    iconUpdateURL = "https://hoogle.haskell.org/favicon.png";
    updateInterval = updateEveryDay;
    definedAliases = [ "@hoogle" ];
  };

  "Kubernetes" = {
    urls = [ { template = "https://kubernetes.io/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://kubernetes.io/images/kubernetes.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@kubernetes"
      "@k8s"
    ];
  };

  "Last.fm" = {
    urls = [ { template = "https://www.last.fm/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.last.fm/static/images/favicon.702b239b6194.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@lastfm" ];
  };

  "MDN" = {
    urls = [ { template = "https://developer.mozilla.org/en-US/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://developer.mozilla.org/favicon-48x48.cbbd161b.png";
    updateInterval = updateEveryDay;
    definedAliases = [ "@mdn" ];
  };

  "MELPA" = {
    urls = [ { template = "https://melpa.org/#/?q={searchTerms}"; } ];
    iconUpdateURL = "https://melpa.org/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@melpa" ];
  };

  "NixOS Packages" = {
    urls = [ { template = "https://search.nixos.org/packages?channel=unstable&query={searchTerms}"; } ];
    iconUpdateURL = "https://nixos.org/favicon.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@nixpkgs"
      "@np"
    ];
  };

  "NixOS Options" = {
    urls = [ { template = "https://search.nixos.org/options?channel=unstable&query={searchTerms}"; } ];
    iconUpdateURL = "https://nixos.org/favicon.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@nixopts"
      "@no"
    ];
  };

  "NixOS Wiki" = {
    urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];
    iconUpdateURL = "https://wiki.nixos.org/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@nixosw" ];
  };

  "NixOS Issues" = {
    urls = [
      { template = "https://github.com/NixOS/nixpkgs/issues?q=is%3Aissue+is%3Aopen+{searchTerms}"; }
    ];
    iconUpdateURL = "https://nixos.org/favicon.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@nixissues"
    ];
  };

  "noogle" = {
    urls = [ { template = "https://noogle.dev/q?term={searchTerms}"; } ];
    iconUpdateURL = "https://noogle.dev/favicon.png";
    updateInterval = updateEveryDay;
    definedAliases = [ "@noogle" ];
  };

  "Home manager options" = {
    urls = [ { template = "https://home-manager-options.extranix.com/?query={searchTerms}"; } ];
    iconUpdateURL = "https://nixos.org/favicon.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@hmopts"
      "@hm"
    ];
  };

  "Mynixos" = {
    urls = [ { template = "https://mynixos.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://mynixos.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@mynixos" ];
  };

  "PyPI" = {
    urls = [ { template = "https://pypi.org/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://pypi.org/static/images/logo-small.2a411bc6.svg";
    updateInterval = updateEveryDay;
    definedAliases = [ "@pypi" ];
  };

  "Python Docs" = {
    urls = [ { template = "https://docs.python.org/3/search.html?q={searchTerms}"; } ];
    iconUpdateURL = "https://docs.python.org/3/_static/py.svg";
    updateInterval = updateEveryDay;
    definedAliases = [ "@pydocs" ];
  };

  "Rust Std" = {
    urls = [ { template = "https://doc.rust-lang.org/std/?search={searchTerms}"; } ];
    iconUpdateURL = "https://www.rust-lang.org/static/images/favicon-32x32.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@ruststd"
      "@rust"
    ];
  };

  "WolframAlpha" = {
    urls = [ { template = "https://www.wolframalpha.com/input?i={searchTerms}"; } ];
    iconUpdateURL = "https://www.wolframalpha.com/_next/static/images/favicon_1zbE9hjk.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@wolframalpha"
      "@wa"
    ];
  };

  "Stack Overflow" = {
    urls = [ { template = "https://stackoverflow.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://cdn.sstatic.net/Sites/stackoverflow/Img/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@stackoverflow"
      "@so"
    ];
  };

  "npm" = {
    urls = [ { template = "https://www.npmjs.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://static.npmjs.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@npm" ];
  };

  "Reddit" = {
    urls = [ { template = "https://www.reddit.com/search/?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.reddit.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@reddit"
      "@r"
    ];
  };

  "YouTube" = {
    urls = [ { template = "https://www.youtube.com/results?search_query={searchTerms}"; } ];
    iconUpdateURL = "https://www.youtube.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@youtube"
      "@yt"
    ];
  };

  "Wikipedia" = {
    urls = [ { template = "https://en.wikipedia.org/w/index.php?search={searchTerms}"; } ];
    iconUpdateURL = "https://en.wikipedia.org/static/favicon/wikipedia.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@wikipedia"
      "@wiki"
    ];
  };

  "Amazon" = {
    urls = [ { template = "https://www.amazon.com/s?k={searchTerms}"; } ];
    iconUpdateURL = "https://www.amazon.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@amazon" ];
  };

  "DuckDuckGo" = {
    urls = [ { template = "https://duckduckgo.com/?q={searchTerms}"; } ];
    iconUpdateURL = "https://duckduckgo.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@duckduckgo"
      "@ddg"
    ];
  };

  "eBay" = {
    urls = [ { template = "https://www.ebay.com/sch/i.html?_nkw={searchTerms}"; } ];
    iconUpdateURL = "https://www.ebay.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@ebay"
      "@eb"
    ];
  };

  "Twitter" = {
    urls = [ { template = "https://twitter.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://twitter.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@twitter"
      "@x"
      "@tw"
    ];
  };

  "olx.ua" = {
    urls = [ { template = "https://www.olx.ua/uk/list/q-{searchTerms}/"; } ];
    iconUpdateURL = "https://www.olx.ua/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@olx" ];
  };

  "Google Fonts" = {
    urls = [ { template = "https://fonts.google.com/?query={searchTerms}"; } ];
    iconUpdateURL = "https://www.gstatic.com/images/icons/material/apps/fonts/1x/catalog/v5/favicon.svg";
    updateInterval = updateEveryDay;
    definedAliases = [ "@goofonts" ];
  };

  "DevDocs" = {
    urls = [ { template = "https://devdocs.io/#q={searchTerms}"; } ];
    iconUpdateURL = "https://devdocs.io/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@devdocs" ];
  };

  "Can I Use" = {
    urls = [ { template = "https://caniuse.com/?search={searchTerms}"; } ];
    iconUpdateURL = "https://caniuse.com/img/favicon-128.png";
    updateInterval = updateEveryDay;
    definedAliases = [ "@caniuse" ];
  };

  "Hugging Face" = {
    urls = [ { template = "https://huggingface.co/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://huggingface.co/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@hf"
      "@huggingface"
    ];
  };

  "Papers With Code" = {
    urls = [ { template = "https://paperswithcode.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://paperswithcode.com/favicon-32x32.png";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@pwc"
      "@paperswithcode"
    ];
  };

  Perplexity = {
    urls = [ { template = "https://www.perplexity.ai/?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.perplexity.ai/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [
      "@p"
      "@perplexity"
    ];
  };

  "Phind" = {
    urls = [ { template = "https://www.phind.com/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.phind.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@phind" ];
  };

  "Qwant" = {
    urls = [ { template = "https://www.qwant.com/?q={searchTerms}"; } ];
    iconUpdateURL = "https://www.qwant.com/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@qwant" ];
  };

  "Arch Wiki" = {
    urls = [ { template = "https://wiki.archlinux.org/index.php?search={searchTerms}"; } ];
    iconUpdateURL = "https://wiki.archlinux.org/favicon.ico";
    updateInterval = updateEveryDay;
    definedAliases = [ "@alwiki" ];
  };

  "SearXNG" = {
    urls = [ { template = "https://search.inetol.net/search?q={searchTerms}"; } ];
    iconUpdateURL = "https://search.inetol.net/favicon.ico";
    updateInterval = updateEveryDay;
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
    updateInterval = updateEveryDay;
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
    updateInterval = updateEveryDay;
    definedAliases = [
      "@bs"
      "@bravesum"
    ];
  };
}
