{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.Map as Map
import Data.Aeson
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.ByteString.Lazy hiding (filter, readFile, zipWith, map, putStrLn)
import qualified Data.ByteString as Unlazy
import Control.Monad.State

import Snap.Extras.JSON

import System.Directory

import Game.Types
import Game.Parser
import Game.Actions

import Data.String.Utils

import Debug.Trace

gameDirectory :: FilePath
gameDirectory = "rooms"

main :: IO ()
main = do
  -- Get and parse all rooms 
  files <- getDirectoryContents gameDirectory
  let roomFiles = filter (endswith ".room") files
      roomFilePaths = map ((gameDirectory ++ "/") ++) roomFiles
  contents <- mapM readFile roomFilePaths
  let rooms = Map.fromList $ zipWith parseRoom contents roomFiles

  -- Create a mutable variable where we store all game state
  state <- newMVar $ initGame rooms

  quickHttpServe $ site state

maxRequestSize = 10000

site :: MVar Game -> Snap ()
site state = do
  let useRoute :: (ByteString -> State Game ByteString) -> Snap ()
      useRoute route = do
        input <- readRequestBody maxRequestSize
        value <- liftIO $ takeMVar state
        let (response, newState) = runState (route input) $ trace ("old " ++ (show $ lastId value)) value
        liftIO $ putMVar state newState
        liftIO $ print $ lastId newState
        trace (show response) $ writeLBS response

  -- Show the main page at the top level
  ifTop mainPage <|>

  -- Serve static files from /static
   (dir "static" $ serveDirectory "static") <|>

  -- Interact with the game server via JSON
   (route $ Map.assocs $ Map.map useRoute routes)

-- JSON endpoints
routes :: Map.Map Unlazy.ByteString (ByteString -> State Game ByteString)
routes = Map.fromList [
    (toStrict "run", runCommand),
    (toStrict "history", getHistory)
  ]

mainPage :: Snap ()
mainPage = serveFile "html/index.html"
