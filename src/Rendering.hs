module Rendering where

import BasicPrelude
import Control.Lens.Operators
import Data.Text (pack)
import Data.Vector (toList)
import Text.Printf (printf)

import Game

data SGR
  = Reset
  | SetColor !Color !Bool -- True for background

-- | Convert a color to its ANSI representation.
colorToCode :: Color -> Int
colorToCode Blue   = 4
colorToCode Red    = 1
colorToCode Yellow = 3
colorToCode Black  = 0
colorToCode White  = 7

csi :: [Int] -> Text -> Text
csi args code = "\ESC[" <> intercalate ";" (map tshow args) <> code

-- | Convert an SGR to its code.
sgrToCode :: SGR -> Int
sgrToCode Reset = 0
sgrToCode (SetColor color False) = colorToCode color + 30
sgrToCode (SetColor color True)  = colorToCode color + 100

-- Get the SGR code for a given list of SGRs.
setSGRCode :: [SGR] -> Text
setSGRCode sgrs = csi (map sgrToCode sgrs) "m"

-- | Render a single card
renderCard :: Bool -> Card -> Text
renderCard z c =
  setSGRCode [sgrs z (c ^. cardRevealed)]
  <> (pack $ printf "%-14s" (c ^. cardText))
  <> (setSGRCode [Reset])
  where
    cardCol = c ^. cardColor
    sgrs True True   = SetColor cardCol True
    sgrs True False  = SetColor cardCol False
    sgrs False True  = SetColor cardCol False
    sgrs False False = SetColor White   False

-- | Render a line of cards.
renderLine :: Bool -> [Card] -> Text
renderLine z = intercalate " " . map (renderCard z)

-- | Divide a list into sublists of given size.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

-- | Render a complete 5x5 board, the Bool is for whether or not the viewer is a Spymaster.
renderBoard :: Bool -> Board -> Text
renderBoard z = intercalate "\n" . map (renderLine z) . chunks 5 . toList

-- | Convert a team to a color.
toCardColor :: Team -> Color
toCardColor BlueTeam = Blue
toCardColor RedTeam  = Red

-- | Render the player's role and team, colored appropriately.
renderPlayer :: Player -> Text
renderPlayer p =
  setSGRCode [SetColor cardCol False]
  <> tshow cardCol
  <> " "
  <> tshow (p ^. playerRole)
  <> setSGRCode [Reset]
  where
    cardCol = toCardColor $ p ^. playerTeam

-- | Render the line saying who's turn it is.
renderTurn :: Player -> Game -> Text
renderTurn p g
  | p == t    = "your"
  | otherwise = "the " <> renderPlayer t <> "'s"
  where
    t = g ^. gameTurn

-- | Render the line containing the current clue.
renderClue :: Clue -> Text
renderClue (c, n) = "The current clue is: " <> c <> " " <> tshow n

-- | Render a team's name and its color.
renderTeam :: Team -> Text
renderTeam t =
  setSGRCode [SetColor col False]
  <> tshow col
  <> " Team"
  <> setSGRCode [Reset]
  where
    col = toCardColor t

-- | Render the text for the winning team on the GameOver screen.
renderWinningTeam :: Player -> Team -> Text
renderWinningTeam p t
  | playerWon =
      "Congrats, you made it! "
      <> renderTeam t
      <> " won!"
  | otherwise =
      "It was a tough match, but eventually "
      <> renderTeam t
      <> " came out on top."
  where
    playerWon = p ^. playerTeam == t

-- | ANSI code to clear the screen.
clearScreen :: Text
clearScreen = csi [1, 1] "H" <> csi [2] "J"

-- | Render the GameOver state.
renderGameOver :: Player -> Game -> Text
renderGameOver p g =
  clearScreen
  <> renderWinningTeam p winningTeam
  <> "\n"
  <> "\n"
  <> renderBoard True (g ^. gameBoard)
  <> "\n"
  where
    (GameOver winningTeam) = g ^. gameState
    playerWon = winningTeam == p ^. playerTeam

-- | Render the game for the player.
renderGame :: Player -> Game -> Text
renderGame p g
  | GameOver t <- g ^. gameState = renderGameOver p g
renderGame p g =
  clearScreen
  <> "Hey there! You are a " <> renderPlayer p <> ", and it is " <> renderTurn p g <> " turn."
  <> "\n"
  <> (if g ^. gameTurn . playerRole /= Spymaster
        then renderClue (g ^. gameClue)
        else "")
  <> "\n"
  <> renderBoard (p ^. playerRole == Spymaster) (g ^. gameBoard)
  <> "\n"
  <> (if g ^. gameTurn == p
        then "> "
        else "")

