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
import Data.ByteString
import Control.Monad.State

import Snap.Extras.JSON

import System.Directory
import Data.String.Utils

import Game.Parser

gameDirectory :: FilePath
gameDirectory = "rooms"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = do
  -- Get and parse all rooms 
  game <- liftIO $ do
    files <- getDirectoryContents gameDirectory
    let roomFiles = filter (endswith ".room") files
    contents <- mapM readFile roomfiles
    return $ Map.fromList $ zipWith parseRoom contents roomFiles

  let initialState = initGame
  -- Create a mutable variable where we store all game state
  state <- liftIO $ newMVar 10

  let useRoute route = do
      input <- getRequestBody
      value <- liftIO $ takeMVar state
      let (response, newState) = runState (route input) value
      liftIO $ putMVar state newState
      writeBS response

  -- Show the main page at the top level
  ifTop mainPage <|>

  -- Serve static files from /static
   (dir "static" $ serveDirectory "static") <|>

  -- Interact with the game server via JSON
   (route $ Map.assocs $ Map.map useRoute routes)

-- JSON endpoints
routes :: Map.Map ByteString (Bytestring -> State Int ByteString)
routes = Map.fromList [
    ("run", runCommand),
    ("history", getHistory)
  ]

mainPage :: Snap ()
mainPage = serveFile "html/index.html"
