module Main where

import BasicPrelude
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Lens.Operators
import Control.Monad
import Data.Random
import Data.Text.IO (hGetLine, hPutStr, hPutStrLn)
import Data.Vector (fromList)
import Network.Socket
import System.IO
  ( BufferMode(NoBuffering)
  , Handle
  , IOMode(ReadWriteMode)
  , hClose
  , hSetBuffering
  )

import Dicts
import Game
import Logic
import Rendering

-- | Generate a random board from the given dictionary.
randomBoard :: Dictionary -> IO Board
randomBoard d = do
  words <- runRVar (shuffleNofM boardSize dictLength d) StdRandom
  let coloredWords = map ($ False) $ zipWith Card words defaultColors
  fmap fromList $ runRVar (shuffleN boardSize coloredWords) StdRandom
  where
    dictLength = length d
    boardSize = length defaultColors

-- | Create a new game using a generated board from the original dictionary and no players.
newGame :: IO Game
newGame = do
  b <- randomBoard originalDict
  return $ Game b (Player BlueTeam Spymaster) ("", 0) Running noPlayers
  where
    noPlayers = GamePlayers False False 0 0

port :: PortNumber
port = 8080

main :: IO ()
main = do
  let hints = defaultHints {addrSocketType = Stream}
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port)
  serverSocket <-
    socket <$> addrFamily <*> addrSocketType <*> addrProtocol $ addr
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket (addrAddress addr)
  listen serverSocket 2
  putStrLn $ "Listening on port " <> tshow port
  game <- newGame >>= (atomically . newTVar)
  forever $ do
    (clientSocket, clientInfo) <- accept serverSocket
    handle <- socketToHandle clientSocket ReadWriteMode
    hSetBuffering handle NoBuffering
    putStrLn $ "Accepted connection from " <> tshow clientInfo
    forkIO (talk handle game)

-- | Initialize connection with the client
talk :: Handle -> TVar Game -> IO ()
talk h game = do
  p <-
    atomically $ do
      g <- readTVar game
      let (gp', p) = addNeededPlayer (g ^. gamePlayers)
      writeTVar game (gamePlayers .~ gp' $ g)
      return p
  c <- atomically newTChan -- A channel for communication between the receive from client and receive from state threads
  catch (void $ race (server h game c p) (receive h c)) $ \(SomeException _) ->
    atomically $
    fmap (gamePlayers %~ removePlayer p) (readTVar game) >>= writeTVar game

-- | Receive from the client, write to the channel (which is read by `newline`)
receive :: Handle -> TChan Text -> IO ()
receive h c =
  forever $ do
    line <- hGetLine h
    atomically $ writeTChan c line

-- | Handle connection to the client.
server :: Handle -> TVar Game -> TChan Text -> Player -> IO ()
server h game c p =
  forever $ do
    g <- atomically (readTVar game)
    hPutStr h $ renderGame p g
    loop g
  where
    loop :: Game -> IO ()
    loop g = do
      action <-
        atomically $ do
          g' <- readTVar game
          if (g /= g')
            then return (newgame g g')
            else do
              l <- readTChan c
              return (newline g l)
      action

    -- | Called when there's a new game, sends it to the client if it's visually different.
    newgame :: Game -> Game -> IO ()
    newgame g g' = do
      when (renderGame p g /= renderGame p g') $ hPutStr h $ renderGame p g'
      loop g'

    -- | Called when the client supplies a new line, writes it to the state.
    newline :: Game -> Text -> IO ()
    newline g s = do
      case (g ^. gameState) of
        Running -> do
          let g' = playTurn p s g
          if (g == g')
            then hPutStr h $ renderGame p g
            else atomically $ writeTVar game g'
        GameOver _ ->
          when (p == Player BlueTeam Spymaster) $
          fmap (gamePlayers .~ (g ^. gamePlayers)) newGame >>=
          (atomically . writeTVar game)
      loop g
