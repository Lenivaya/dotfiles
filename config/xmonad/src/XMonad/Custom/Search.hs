module XMonad.Custom.Search
  ( mySearch
  ) where

import           Data.List
import           XMonad
import           XMonad.Actions.Search
import           XMonad.Actions.ShowText
import           XMonad.Custom.Prompt
import           XMonad.Prompt

myEngines :: [SearchEngine]
myEngines =
  [ alpha
  , mathworld
  , wikipedia
  , cratesIo
  , flora
  , imdb
  , hackage
  , hoogle
  , dictionary
  , thesaurus
  , vocabulary
  , rosettacode
  , github
  , youtube
  , prefixAware google
  ]

engineNames :: [String]
engineNames = map getName myEngines where getName (SearchEngine name _) = name

myEngine = intelligent . namedEngine "multi" $ foldr1 (!>) myEngines

showAllEngines :: X ()
showAllEngines = flashText helpPromptConfig 0.5 prompt
  where prompt = unwords engineNames

mySearch :: X ()
mySearch =
  showAllEngines >> promptSearch (promptNoCompletion promptTheme) myEngine

