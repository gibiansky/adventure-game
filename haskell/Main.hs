{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.Map as Map
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.ByteString.Lazy hiding (filter, readFile, zipWith, map, putStrLn, zip)
import qualified Data.ByteString as Unlazy
import Control.Monad.State

import System.Directory

import Game.Types
import Game.Parser
import Game.Actions

import Data.String.Utils

import Debug.Trace
import GHC.Int(Int64)

gameDirectory :: FilePath
gameDirectory = "rooms"

eventDirectory :: FilePath
eventDirectory = "events"

wrapInScriptTag :: String -> String
wrapInScriptTag javascript = filter (/= '\n') $ "<script>" ++ javascript ++ "</script>"

main :: IO ()
main = do
  -- Get and parse all rooms 
  files <- getDirectoryContents gameDirectory
  let roomFiles = filter (endswith ".room") files
      roomFilePaths = map ((gameDirectory ++ "/") ++) roomFiles
  roomContents <- mapM readFile roomFilePaths
  let gameRooms = Map.fromList $ zipWith parseRoom roomContents roomFiles

  allEventFiles <- getDirectoryContents eventDirectory
  let eventFiles = filter (endswith ".js") allEventFiles
      eventNames = map (replace ".js" "") eventFiles
      eventFilePaths = map ((eventDirectory ++ "/") ++) eventFiles
  eventContents <- mapM readFile eventFilePaths
  let gameEvents = Map.fromList $ zip eventNames $ map wrapInScriptTag eventContents

  print gameEvents

  -- Create a mutable variable where we store all game state
  state <- newMVar $ initGame gameRooms gameEvents

  quickHttpServe $ site state

maxRequestSize ::  GHC.Int.Int64
maxRequestSize = 10000

site :: MVar Game -> Snap ()
site state = do
  let useRoute :: (ByteString -> State Game ByteString) -> Snap ()
      useRoute rt = do
        input <- readRequestBody maxRequestSize
        value <- liftIO $ takeMVar state
        let (response, newState) = runState (rt input) value
        liftIO $ putMVar state newState
        liftIO $ print $ lastId newState
        trace (show response) $ writeLBS response

  -- Serve static files from /static
  let staticDir = dir "static" $ serveDirectory "static"
      -- Show the main page at the top level
      mainSite = ifTop mainPage
      -- Interact with the game server via JSON
      otherRoutes = route $ Map.assocs $ Map.map useRoute routes

  mainSite <|> staticDir <|> otherRoutes

-- JSON endpoints
routes :: Map.Map Unlazy.ByteString (ByteString -> State Game ByteString)
routes = Map.fromList [
    (toStrict "run", runCommand),
    (toStrict "history", getHistory),
    (toStrict "items", getItems)
  ]

mainPage :: Snap ()
mainPage = serveFile "html/index.html"
