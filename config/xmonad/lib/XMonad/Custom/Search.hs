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
    homeManager,
    duckduckgo,
    images,
    maps,
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
      "https://github.com/search?type=code&q={searchTerms}",
    searchEngine
      "mdn"
      "https://developer.mozilla.org/en-US/search?q=",
    prefixAware
      google
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
