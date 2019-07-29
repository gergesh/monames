{-# LANGUAGE TemplateHaskell #-}

module Game where

import BasicPrelude
import Control.Lens
import Data.Vector (Vector)

data Team = BlueTeam | RedTeam
            deriving (Eq, Show)

otherTeam :: Team -> Team
otherTeam BlueTeam = RedTeam
otherTeam RedTeam = BlueTeam

data Role = Spymaster | Guesser
            deriving (Eq, Show)

data Player = Player { _playerTeam :: Team
                     , _playerRole :: Role
                     } deriving (Eq, Show)
makeLenses ''Player

data Color = Blue
           | Red
           | Yellow
           | Black
           | White
           deriving (Eq, Show)

colorMatches :: Team -> Color -> Bool
colorMatches BlueTeam Blue = True
colorMatches RedTeam  Red  = True
colorMatches _        _    = False

data Card = Card { _cardText     :: Text
                 , _cardColor    :: Color
                 , _cardRevealed :: Bool
                 } deriving (Eq, Show)
makeLenses ''Card

data State = Running | GameOver Team
             deriving (Eq, Show)

type Board = Vector Card

type Clue = (Text, Int)

data GamePlayers = GamePlayers { _hasBlueSpymaster :: Bool
                               , _hasRedSpymaster  :: Bool
                               , _numBlueGuessers  :: Int
                               , _numRedGuessers   :: Int
                               } deriving (Eq, Show)
makeLenses ''GamePlayers

data Game = Game { _gameBoard   :: Board
                 , _gameTurn    :: Player
                 , _gameClue    :: Clue
                 , _gameState   :: State
                 , _gamePlayers :: GamePlayers
                 } deriving (Eq, Show)
makeLenses ''Game

defaultColors :: [Color]
defaultColors = replicate 8 Blue
             ++ replicate 7 Red
             ++ replicate 9 Yellow
             ++ replicate 1 Black
