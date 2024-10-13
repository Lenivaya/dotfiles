module XMonad.Custom.Search (
  mySearch,
  selectAndSearchPrompt,
) where

import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
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
  showAllEngines
  mkXPrompt
    SearchEngineByName
    conf
    (mkComplFunFromList' conf engineNames)
    (selectAndSearch . fromMaybe duckduckgo . (`M.lookup` namesToEnginesMap))
  where
    selectAndSearch engine = promptSearch (promptNoHistory conf) engine
