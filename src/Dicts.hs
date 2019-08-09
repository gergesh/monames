{-# LANGUAGE TemplateHaskell #-}

module Dicts where

import           Data.FileEmbed
import           Data.Text

type Dictionary = [Text]

loadDictFile :: Text -> Dictionary
loadDictFile = lines

originalDict, duetDict, undercoverDict :: Dictionary
originalDict   = loadDictFile $(embedStringFile "src/dictionaries/original.txt")
duetDict       = loadDictFile $(embedStringFile "src/dictionaries/duet.txt")
undercoverDict = loadDictFile $(embedStringFile "src/dictionaries/undercover.txt")
