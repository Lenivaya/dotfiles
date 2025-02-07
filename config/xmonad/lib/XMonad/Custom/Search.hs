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
import XMonad.Actions.ShowText
import XMonad.Custom.Prompt
import XMonad.Prompt

reddit = searchEngine "reddit" "https://www.reddit.com/search/?q="
nixpkgs = searchEngine "nixpkgs" "https://search.nixos.org/packages?&query="
nixopts = searchEngine "nixopts" "https://search.nixos.org/options?&query="
searx = searchEngine "searx" "https://searx.work/?q="
nixpkgsissues = searchEngine "nixpkgsissues" "https://github.com/NixOS/nixpkgs/issues?q="
archWiki = searchEngine "archWiki" "https://wiki.archlinux.org/index.php?search="
phind = searchEngine "phind" "https://www.phind.com/search?q="
perplexity = searchEngine "perplexity" "https://www.perplexity.ai/search?s=o&q="
qwant = searchEngine "qwant" "https://www.qwant.com/?q="
mynixos = searchEngine "mynixos" "https://mynixos.com/search?q="
nixosWiki = searchEngine "nixosWiki" "https://wiki.nixos.org/w/index.php?search="
brave = searchEngine "brave" "https://search.brave.com/search?q="
aliexpress = searchEngine "aliexpress" "https://www.aliexpress.com/wholesale?SearchText="
docker = searchEngine "docker" "https://hub.docker.com/search?q="
githubcode = searchEngine "githubcode" "https://github.com/search?type=code&q="
genius = searchEngine "genius" "https://genius.com/search?q="
nixosPr = searchEngine "nixosPr" "https://github.com/NixOS/nixpkgs/pulls?q="
twitter = searchEngine "twitter" "https://twitter.com/search?q="
olxua = searchEngine "olxua" "https://www.olx.ua/uk/list/q-"
translate = searchEngine "translate" "https://translate.google.com/?sl=auto&tl=en&text="
tradingview = searchEngine "tradingview" "https://www.tradingview.com/symbols/"
goodreads = searchEngine "goodreads" "https://www.goodreads.com/search?q="
alternativeto = searchEngine "alternativeto" "https://alternativeto.net/browse/search?q="
awesomeLists = searchEngine "awesomelists" "https://awesomelists.top/search?q="
unsplash = searchEngine "unsplash" "https://unsplash.com/s/photos/"
downdetector = searchEngine "downdetector" "https://downdetector.com/status/"
lazyvim = searchEngine "lazyvim" "https://www.lazyvim.org/search?q="

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
    reddit,
    homeManager,
    nixpkgs,
    nixpkgsissues,
    nixopts,
    searx,
    duckduckgo,
    images,
    maps,
    archWiki,
    phind,
    perplexity,
    qwant,
    mynixos,
    nixosWiki,
    brave,
    aliexpress,
    docker,
    githubcode,
    genius,
    nixosPr,
    twitter,
    olxua,
    translate,
    tradingview,
    goodreads,
    alternativeto,
    awesomeLists,
    unsplash,
    downdetector,
    lazyvim,
    prefixAware google
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
