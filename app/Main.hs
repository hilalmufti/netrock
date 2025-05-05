{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main (main) where


import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever, void)

import Data.Time (getCurrentTime)
import qualified Data.List.NonEmpty as NE

import System.Environment (getArgs)
import System.IO

import Network.Socket

logo :: [String]
logo = [
  "⠀⠀⠀⠀⠀⣠⡴⠖⠒⠲⠶⢤⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⡴⠖⠒⢶⣄⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠀⠀⢀⡾⠁⠀⣀⠔⠁⠀⠀⠈⠙⠷⣤⠦⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡼⠋⠀⠀⠀⢀⡿⠀⠀⠀⠀⠀⠀⠀",
  "⣠⠞⠛⠛⠛⠋⠉⠀⠀⠀⠀⠀⠀⠀⠀⠘⢧⠈⢿⡀⢠⡶⠒⠳⠶⣄⠀⠀⠀⠀⠀⣴⠟⠁⠀⠀⠀⣰⠏⠀⢀⣤⣤⣄⡀⠀⠀",
  "⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠟⠛⠛⠃⠸⡇⠈⣇⠸⡇⠀⠀⠀⠘⣇⠀⠀⣠⡾⠁⠀⠀⠀⢀⣾⣣⡴⠚⠉⠀⠀⠈⠹⡆⠀",
  "⣹⡷⠤⠤⠤⠄⠀⠀⠀⠀⢠⣤⡤⠶⠖⠛⠀⣿⠀⣿⠀⢻⡄⠀⠀⠀⢻⣠⡾⠋⠀⠀⠀⠀⣠⡾⠋⠁⠀⠀⠀⠀⢀⣠⡾⠃⠀",
  "⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⡤⠖⠋⢀⣿⣠⠏⠀⠀⣿⠀⠀⠀⠘⠉⠀⠀⠀⠀⠀⡰⠋⠀⠀⠀⠀⠀⣠⠶⠋⠁⠀⠀⠀",
  "⢿⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⡾⠋⠁⠀⠀⠠⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⠁⠀⠀⠀⢀⣴⡿⠥⠶⠖⠛⠛⢶⡄",
  "⠀⠉⢿⡋⠉⠉⠁⠀⠀⠀⠀⠀⢀⣠⠾⠋⠀⠀⠀⠀⢀⣰⡇⠀⠀⢀⡄⠀⠀⠀⠀⠀⠀⠀⠀⢀⡴⠋⠀⠀⠀⠀⠀⢀⣠⠼⠃",
  "⠀⠀⠈⠛⠶⠦⠤⠤⠤⠶⠶⠛⠋⠁⠀⠀⠀⠀⠀⠀⣿⠉⣇⠀⡴⠟⠁⣠⡾⠃⠀⠀⠀⠀⠀⠈⠀⠀⠀⣀⣤⠶⠛⠉⠀⠀⠀",
  "⠀⠀⠀⠀⢀⣠⣤⣀⣠⣤⠶⠶⠒⠶⠶⣤⣀⠀⠀⠀⢻⡄⠹⣦⠀⠶⠛⢁⣠⡴⠀⠀⠀⠀⠀⠀⣠⡶⠛⠉⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠀⢀⡴⠋⣠⠞⠋⠁⠀⠀⠀⠀⠙⣄⠀⠙⢷⡀⠀⠀⠻⣄⠈⢷⣄⠈⠉⠁⠀⠀⠀⢀⣠⡴⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⢀⡾⠁⣴⠋⠰⣤⣄⡀⠀⠀⠀⠀⠈⠳⢤⣼⣇⣀⣀⠀⠉⠳⢤⣭⡿⠒⠶⠶⠒⠚⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⢸⠃⢰⠇⠰⢦⣄⡈⠉⠀⠀⠀⠀⠀⠀⠀⠀⠈⠉⠉⠉⠛⠛⠓⠲⢦⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠸⣧⣿⠀⠻⣤⡈⠛⠳⠆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢻⡆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠀⠈⠹⣆⠀⠈⠛⠂⠀⠀⠀⠀⠀⠀⠈⠐⠒⠒⠶⣶⣶⠶⠤⠤⣤⣠⡼⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠀⠀⠀⠹⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⠳⢦⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠀⠀⠀⠀⠈⠻⣦⣀⠀⠀⠀⠀⠐⠲⠤⣤⣀⡀⠀⠀⠀⠀⠀⠉⢳⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠶⠤⠤⠤⠶⠞⠋⠉⠙⠳⢦⣄⡀⠀⠀⠀⡷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
  "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠙⠳⠦⠾⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
    ]

usage :: [String]
usage = [
  "usage: netrock [options] port",
  "",
  "netrock: play rock-paper-scissors over a tcp connection",
  "",
  "Options:",
  "  -h, --help  Show this help message and exit",
  "",
  "Arguments:",
  "  port        The port number to play your game over",
  "",
  "Examples:",
  "  -- Start the server on port 3000",
  "  netrock 3000",
  "",
  "  -- In a separate terminal, now simply connect to the server using netcat",
  "  nc localhost 3000"
        ]

backlog :: Int
backlog = 1024

data Move = Rock | Paper | Scissors
  deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

type PortID = ServiceName

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound .. maxBound]

allMoves :: [Move]
allMoves = allValues

outcome :: Move -> Move -> Outcome
outcome Rock Scissors  = Win
outcome Scissors Paper = Win
outcome Paper Rock     = Win
outcome m m' | m == m' = Tie
outcome m m'           = Lose

parseRead :: Read a => String -> Maybe a
parseRead s
  | [(x, rest)] <- reads s, ok rest = Just x
  | otherwise                       = Nothing
  where ok = all (`elem` (" \r\n" :: String))

parseMove :: String -> Maybe Move
parseMove = parseRead

readMove :: Handle -> IO Move
readMove h = do
    hPutStrLn h $ "Please enter one of " ++ show allMoves
    line <- hGetLine h
    case parseMove line of
      Nothing    -> readMove h
      Just m'    -> return m'

computerVsUser :: Move -> Handle -> IO ()
computerVsUser m h = do
  m' <- readMove h
  hPutStrLn h $ "You " <> show (outcome m' m)

timestamp :: String -> IO String
timestamp s = do
  datetime <- getCurrentTime
  let stamped = "[" <> show datetime <> "]" <> " " <> s
  return stamped

timestampEcho :: Handle -> IO ()
timestampEcho h = do
  hPutStrLn h "Please enter a message"
  hPutStr h "> "
  hFlush h
  msg <- hGetLine h
  stamped <- timestamp msg
  hPutStrLn h stamped
  timestampEcho h

withTty :: (Handle -> IO a) -> IO a
withTty = withFile "/dev/tty" ReadWriteMode

withClient :: PortID -> (Handle -> IO a) -> IO a
withClient port server = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) Nothing (Just port)
    let addr = NE.head addrs

    let loop sock = forever $ bracketOnError (accept sock) (close . fst) $
          \(conn, _peer) -> void $ do
            putStrLn "Connected!"
            h <- socketToHandle conn ReadWriteMode
            forkFinally (server h) (\_ -> hClose h)

    bracket (open addr) close loop

  where
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      bind sock $ addrAddress addr
      listen sock backlog
      putStrLn "Server running..."
      return sock

play :: MVar Move -> MVar Move -> Handle -> IO ()
play myMove opMove h = do
    m <- readMove h

    isMyEmpty <- isEmptyMVar myMove
    isOpEmpty <- isEmptyMVar opMove
    if isMyEmpty then
      putMVar myMove m
    else
      putMVar opMove m

    my <- readMVar myMove
    op <- readMVar opMove

    let outcome' = if m == my then outcome my op else outcome op my

    hPutStrLn h $ "You " <> show outcome'
  where showMove :: MVar Move -> IO String
        showMove m = do
          isEmpty <- isEmptyMVar m
          if isEmpty then
            return "Empty"
          else do
            move <- readMVar m
            return $ show move

netrock :: PortID -> IO ()
netrock port = do
  mapM_ putStrLn logo
  myMove <- newEmptyMVar
  opMove <- newEmptyMVar
  withClient port $ play myMove opMove

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]     -> mapM_ putStrLn usage
    ["--help"] -> mapM_ putStrLn usage
    [port]     -> netrock port
    _          -> mapM_ putStrLn usage

-- TODO:
-- [x] feat: implement handle-based server
-- [x] feat: implement `withClient`
-- [x] feat: play over server against fixed computer
-- [x] feat: timestampEcho over tty
-- [ ] feat: server counts number of connections
-- [ ] feat: server decrements connections on disconnect
-- [ ] feat: play against random computer
-- [ ] feat: catsay winning messages
-- [ ] feat: nicer printing
-- [ ] feat: play multiple rounds
-- [ ] feat: server logging
-- [x] feat: proper CLI
-- [x] feat: play against opponent over server
-- [ ] fix: if server closes, then client closes
-- [ ] fix: server closing logic
