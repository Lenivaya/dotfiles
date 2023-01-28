module XMonad.Custom.Search
  ( mySearch
  ) where

import           XMonad
import           XMonad.Actions.Search
import           XMonad.Custom.Prompt           ( promptTheme )
import           XMonad.Prompt

myEngine = intelligent . namedEngine "multi" $ foldr1
  (!>)
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

mySearch = promptSearch promptTheme { autoComplete = Nothing } myEngine
