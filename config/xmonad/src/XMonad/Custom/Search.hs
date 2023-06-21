module XMonad.Custom.Search (
  mySearch,
  selectAndSearchPrompt,
) where

import Data.Foldable
import Data.List
import XMonad
import XMonad.Actions.Search
import XMonad.Actions.ShowText
import XMonad.Custom.Prompt
import XMonad.Prompt

reddit = searchEngine "reddit" "https://www.reddit.com/search/?q="
nixpkgs = searchEngine "nixpkgs" "https://search.nixos.org/packages?&query="
nixopts = searchEngine "nixopts" "https://search.nixos.org/options?&query="
searx = searchEngine "searx" "https://searx.work/?q="

myEngines :: [SearchEngine]
myEngines =
  [ alpha
  , mathworld
  , wikipedia
  , wiktionary
  , cratesIo
  , flora
  , imdb
  , hackage
  , hoogle
  , noogle
  , dictionary
  , thesaurus
  , vocabulary
  , rosettacode
  , github
  , youtube
  , reddit
  , nixpkgs
  , nixopts
  , searx
  , duckduckgo
  , images
  , maps
  , prefixAware google
  ]

engineNames :: [String]
engineNames = getName <$> myEngines
  where
    getName (SearchEngine name _) = name

myEngine = intelligent . namedEngine "multi" $ foldr1 (!>) myEngines

showAllEngines :: X ()
showAllEngines = flashText helpPromptConfig 0.5 prompt
  where
    prompt = unwords engineNames

mySearch :: X ()
mySearch =
  showAllEngines
    >> promptSearch (promptNoCompletion promptTheme) myEngine

{-
Let's define a prompt that starts with selecting
engine and only then entering search term
-}

namesToEngines = zip engineNames myEngines

data SearchEngineByName = SearchEngineByName

instance XPrompt SearchEngineByName where
  showXPrompt _ = "Search engine: "

selectAndSearchPrompt :: XPConfig -> X ()
selectAndSearchPrompt conf =
  showAllEngines
    >> mkXPrompt
      SearchEngineByName
      conf
      (listCompFunc conf engineNames)
      go
  where
    go selected =
      forM_
        (lookup selected namesToEngines)
        (promptSearch conf)
