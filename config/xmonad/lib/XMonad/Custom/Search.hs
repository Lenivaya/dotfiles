{-# LANGUAGE ImportQualifiedPost #-}

module XMonad.Custom.Search (
  mySearch,
  selectAndSearchPrompt,
) where

import Data.Foldable
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import XMonad
import XMonad.Actions.Search
import XMonad.Actions.Search (searchEngine)
import XMonad.Actions.ShowText
import XMonad.Custom.Prompt
import XMonad.Prompt

myEngines :: [SearchEngine]
myEngines =
  [ alpha,
    mathworld,
    wikipedia,
    wiktionary,
    cratesIo,
    flora,
    imdb,
    hackage,
    hoogle,
    noogle,
    dictionary,
    thesaurus,
    vocabulary,
    rosettacode,
    github,
    youtube,
    duckduckgo,
    images,
    maps,
    searchEngine "homeManager" "https://home-manager-options.extranix.com/?query=",
    searchEngine "nixpkgs" "https://search.nixos.org/packages?&query=",
    searchEngine "nixpkgsissues" "https://github.com/NixOS/nixpkgs/issues?q=",
    searchEngine "nixopts" "https://search.nixos.org/options?&query=",
    searchEngine "searx" "https://searx.work/?q=",
    searchEngine "reddit" "https://www.reddit.com/search/?q=",
    searchEngine "archWiki" "https://wiki.archlinux.org/index.php?search=",
    searchEngine "phind" "https://www.phind.com/search?q=",
    searchEngine "perplexity" "https://www.perplexity.ai/search?s=o&q=",
    searchEngine "qwant" "https://www.qwant.com/?q=",
    searchEngine "mynixos" "https://mynixos.com/search?q=",
    searchEngine "nixosWiki" "https://wiki.nixos.org/w/index.php?search=",
    searchEngine "brave" "https://search.brave.com/search?q=",
    searchEngine "aliexpress" "https://www.aliexpress.com/wholesale?SearchText=",
    searchEngine "docker" "https://hub.docker.com/search?q=",
    searchEngine "githubcode" "https://github.com/search?type=code&q=",
    searchEngine "genius" "https://genius.com/search?q=",
    searchEngine "nixosPr" "https://github.com/NixOS/nixpkgs/pulls?q=",
    searchEngine "twitter" "https://twitter.com/search?q=",
    searchEngine "olxua" "https://www.olx.ua/uk/list/q-",
    searchEngine "translate" "https://translate.google.com/?sl=auto&tl=en&text=",
    searchEngine "tradingview" "https://www.tradingview.com/symbols/",
    searchEngine "goodreads" "https://www.goodreads.com/search?q=",
    searchEngine "alternativeto" "https://alternativeto.net/browse/search?q=",
    searchEngine "awesomelists" "https://awesomelists.top/search?q=",
    searchEngine "unsplash" "https://unsplash.com/s/photos/",
    searchEngine "downdetector" "https://downdetector.com/status/",
    searchEngine "lazyvim" "https://www.lazyvim.org/search?q=",
    searchEngine "nixprtrack" "https://nixpkgs-tracker.ocfox.me/?pr=",
    searchEngine "nixissues" "https://github.com/NixOS/nixpkgs/issues?q=",
    searchEngine
      "nixPullRequests"
      "https://github.com/NixOS/nixpkgs/pulls?q=",
    searchEngine
      "githubCode"
      "https://github.com/search?type=code&q=",
    searchEngine
      "gitlab"
      "https://gitlab.com/search",
    searchEngine
      "sourcehut"
      "https://sr.ht/projects?search=",
    searchEngine
      "sourceGraph"
      "https://sourcegraph.com/search?q=",
    searchEngine
      "genius"
      "https://genius.com/search?q=",
    searchEngine
      "mdn"
      "https://developer.mozilla.org/en-US/search?q=",
    searchEngine
      "lastfm"
      "https://www.last.fm/search?q=",
    searchEngine
      "npm"
      "https://www.npmjs.com/search?q=",
    searchEngine
      "pypi"
      "https://pypi.org/search/?q=",
    searchEngine
      "python"
      "https://docs.python.org/3/search.html?q=",
    searchEngine
      "googleFonts"
      "https://fonts.google.com/?query=",
    searchEngine
      "canIUse"
      "https://caniuse.com/?search=",
    searchEngine
      "huggingFace"
      "https://huggingface.co/search?q=",
    searchEngine
      "huggingFaceModels"
      "https://huggingface.co/models",
    searchEngine
      "devDocs"
      "https://devdocs.io/#q=",
    searchEngine "flatHub" "https://flathub.org/apps/search?q=",
    searchEngine "hackerNews" "https://hn.algolia.com/",
    searchEngine "ubuntuPackages" "https://packages.ubuntu.com/search?keywords=",
    searchEngine "debinaPackages" "https://packages.debian.org/search?keywords=",
    searchEngine "archPackages" "https://archlinux.org/packages/?q=",
    searchEngine "waybackMachine" "https://web.archive.org/web/*/",
    searchEngine "man pages" "https://man.archlinux.org/search?q=",
    searchEngine "httpStatus" "https://httpstatuses.com/",
    searchEngine "emojipedia" "https://emojipedia.org/search/?q=",
    searchEngine "flakehub" "https://flakehub.com/flakes?q=",
    searchEngine "explainShell" "https://explainshell.com/explain?cmd=",
    searchEngine "cheatsh" "https://cheat.sh/",
    searchEngine "arxiv" "https://arxiv.org/search/?searchtype=all&query=",
    searchEngine "nuget" "https://www.nuget.org/packages?q=",
    searchEngine "recipeSearch" "https://recipe-search.typesense.org/?q=",
    searchEngine "whoisLookup" "https://who.is/whois/{searchTerms}",
    searchEngine "dnsLookup" "https://dns.google/query?name={searchTerms}",
    searchEngine "downDetector" "https://downdetector.com/status/",
    searchEngine "pkgsGoDev" "https://pkg.go.dev/search?q=",
    searchEngine
      "google"
      "https://www.google.com/search?q="
  ]

engineNames :: [String]
engineNames = getName <$> myEngines
  where
    getName (SearchEngine name _) = name

myEngine = intelligent . namedEngine "multi" $ foldr1 (!>) myEngines

showAllEngines :: X ()
showAllEngines = flashText helpPromptConfig 0.5 (unwords engineNames)

mySearch :: X ()
mySearch =
  showAllEngines
    >> promptSearch (promptNoCompletion promptTheme) myEngine

{-
Let's define a prompt that starts with selecting
engine and only then entering search term
-}

namesToEngines :: [(String, SearchEngine)]
namesToEngines = zip engineNames myEngines

namesToEnginesMap :: M.Map String SearchEngine
namesToEnginesMap = M.fromList namesToEngines

data SearchEngineByName = SearchEngineByName

instance XPrompt SearchEngineByName where
  showXPrompt _ = "Search engine: "

selectAndSearchPrompt :: XPConfig -> X ()
selectAndSearchPrompt conf = do
  -- showAllEngines
  mkXPrompt
    SearchEngineByName
    conf
    (mkComplFunFromList' conf engineNames)
    (selectAndSearch . fromMaybe duckduckgo . (`M.lookup` namesToEnginesMap))
  where
    selectAndSearch = promptSearch (promptNoHistory conf)
