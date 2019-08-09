module Logic where

import           BasicPrelude
import           Control.Lens                   ( view )
import           Control.Lens.At                ( ix )
import           Control.Lens.Operators
import           Data.Text                      ( pack )
import qualified Data.Vector                   as V
import           Text.Parsec                    ( many1
                                                , parse
                                                )
import           Text.Parsec.Char               ( char
                                                , digit
                                                , letter
                                                , space
                                                )
import           Text.Parsec.Text               ( Parser )

import           Game

-- | Find a card's index by its text.
cardIndexByText :: Text -> Board -> Maybe Int
cardIndexByText w = V.findIndex ((== w) . view cardText)

-- | Check if all cards of the color have been revealed.
areAllRevealed :: Color -> Board -> Bool
areAllRevealed c =
  V.all (view cardRevealed) . V.filter ((== c) . view cardColor)

-- | Check if someone won the game.
checkWinner :: Game -> Maybe Team
checkWinner g | allRev Black = Just . otherTeam $ g ^. gameTurn . playerTeam
              | allRev Blue  = Just BlueTeam
              | allRev Red   = Just RedTeam
              | otherwise    = Nothing
  where allRev col = areAllRevealed col (g ^. gameBoard)

-- | Set a game's State appropriately.
checkGameOver :: Game -> Game
checkGameOver g = case checkWinner g of
  Just t  -> gameState .~ GameOver t $ g
  Nothing -> g

-- | Parse a clue from the spymaster. Has to be one word and one number.
parseClue :: Parser Clue
parseClue = do
  word <- many1 letter
  space
  n <- many1 digit <|> fmap return (char '-')
  let n' = case n of
        "-" -> Nothing
        _   -> Just $ read . pack $ n
  return $ Clue (pack word) n'

-- | Simulate the Spymaster giving a clue.
giveClue :: Text -> Game -> Game
giveClue w g
  | Left _ <- parseResult
  = g
  | Right (Clue _ (Just 0)) <- parseResult
  = g
  | Right clue <- parseResult
  = g & gameTurn . playerRole .~ Guesser & gameClue .~ clue
  where parseResult = parse parseClue "parseClue" w

-- | Simulate the process of a player guessing a word, and either subtract a guess or reset them
guessWord :: Text -> Game -> Game
guessWord w g | w == "-" = gameClue . clueCount .~ Just 0 $ g
              | -- Forfeit additional guesses.
                cardIndexByText w (g ^. gameBoard) == Nothing = g
guessWord w g
  | g ^. gameBoard ^?! ix idx . cardRevealed = g
  | otherwise                                = g & gameBoard . ix idx . cardRevealed .~ True
                                                 & gameClue . clueCount %~ fmap modifierFun
 where
  (Just idx)  = cardIndexByText w (g ^. gameBoard)
  cardCol     = ((g ^. gameBoard) V.! idx) ^. cardColor
  currentTeam = g ^. gameTurn ^. playerTeam
  guessedGood = colorMatches currentTeam cardCol
  modifierFun = if guessedGood then subtract 1 else const 0

-- | Add the needed player to the GamePlayers instance and return him.
addNeededPlayer :: GamePlayers -> (GamePlayers, Player)
addNeededPlayer gp
  | not $ gp ^. hasBlueSpymaster
  = (hasBlueSpymaster .~ True $ gp, Player BlueTeam Spymaster)
  | not $ gp ^. hasRedSpymaster
  = (hasRedSpymaster .~ True $ gp, Player RedTeam Spymaster)
  | otherwise
  = (countSmallerTeam +~ 1 $ gp, Player smallerTeam Guesser)
 where
  (countSmallerTeam, smallerTeam) =
    if gp ^. numBlueGuessers <= gp ^. numRedGuessers
      then (numBlueGuessers, BlueTeam)
      else (numRedGuessers, RedTeam)

-- | Remove a player from the game.
removePlayer :: Player -> GamePlayers -> GamePlayers
removePlayer (Player BlueTeam Spymaster) = hasBlueSpymaster .~ False
removePlayer (Player RedTeam  Spymaster) = hasRedSpymaster .~ False
removePlayer (Player BlueTeam Guesser  ) = numBlueGuessers -~ 1
removePlayer (Player RedTeam  Guesser  ) = numRedGuessers -~ 1

-- | Play a player's turn, match based on his role and whether or not it's his turn.
playerTurn :: Player -> Text -> Game -> Game
playerTurn p w g | p /= g ^. gameTurn = g
                 | r == Spymaster     = giveClue w g
                 | r == Guesser       = guessWord w g
  where r = p ^. playerRole

-- | Check if the guessing team has run out of clues.
checkNeedsNewClue :: Game -> Game
checkNeedsNewClue g = if g ^. gameClue . clueCount == Just 0
  then gameTurn %~ enemySpymaster $ g
  else g
  where enemySpymaster = (playerRole .~ Spymaster) . (playerTeam %~ otherTeam)

-- | Perform post-turn checks. Is the game over? Is it the other team's turn?
postTurn :: Game -> Game
postTurn = checkNeedsNewClue . checkGameOver

-- | Play a complete turn and hand over a new game.
playTurn :: Player -> Text -> Game -> Game
playTurn p w g =
  let g' = playerTurn p w g in if g' /= g then postTurn g' else g
