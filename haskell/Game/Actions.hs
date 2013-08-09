{-# LANGUAGE OverloadedStrings #-}
module Game.Actions where

import Game.Types

import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString hiding (concat, filter, last, repeat, find)
import qualified Data.ByteString.Lazy as Lazy
import Control.Applicative
import Data.Maybe
import Data.Aeson
import Data.List

import Data.String.Utils
import qualified Data.Map as Map

import Debug.Trace
debug thing = trace (show thing) thing

runCommand :: Lazy.ByteString -> State Game Lazy.ByteString
runCommand commandStr = 
  let Just command = decode commandStr in
      do
        modify $ run command
        gets (encode . last . history)

getHistory :: Lazy.ByteString -> State Game Lazy.ByteString
getHistory _ = do
  commands <- gets history
  return $ encode commands


noSuchCommand :: CommandResponse
noSuchCommand = Just "error: no matching command found. enqueue headpat."

instance ToJSON Command where
  toJSON (Command id cmd Nothing) = toJSON $ Command id cmd $ Just "No response."
  toJSON (Command id cmd (Just response)) = object ["id" .= id, "command" .= cmd, "response" .= response]

instance FromJSON Command where
  parseJSON (Object v) = Command <$> return (-1) <*> v .: "command" <*> return Nothing
  parseJSON _ = mzero

initGame :: Map.Map String Room -> Game
initGame rooms =
  let initRoom@(Room enterActions _) = fromJust $ Map.lookup "init" rooms
      game = Game {
        history = [],
        commandCounts = Map.empty,
        currentRoom = fromJust $ Map.lookup "init" rooms,
        rooms = rooms,
        lastId = 1,
        powers = []
      }
      (initializedGame, initOut) = runWriter $ foldM runAction game enterActions in
    initializedGame { history = [Command 0 "start" $ Just initOut]}

setOrModify :: Ord k => a -> (a -> a) -> k -> Map.Map k a -> Map.Map k a
setOrModify val modifier key map =
  case Map.lookup key map of
       Nothing -> Map.insert key val map
       Just oldval -> Map.insert key (modifier oldval) map

run :: Command -> Game -> Game
run command@(Command _ cmdstr _) game = 
  case find (powerMatches cmdstr) (powers game) :: Maybe Power of
       Nothing -> 
         let errcmd = Command (lastId game) cmdstr noSuchCommand in
           game {history = history game ++ [errcmd], lastId = 1 + lastId game}
       Just (Power powname _ actions) -> 
         let (game', cmdOutput) = runWriter $ foldM runAction game actions
             newCommand = Command (lastId game) cmdstr $ Just cmdOutput
             newCounts = setOrModify 1 (+1) powname $ commandCounts game' 
             game'' = game' {history = history game ++ [newCommand], lastId = 1 + lastId game, commandCounts = newCounts} in
          game''

powerMatches :: PowerName -> Power -> Bool
powerMatches str (Power name args _) =
  words str == name : args

getPowerWithName :: String -> Game -> Power
getPowerWithName name game = fromJust $ find ((name ==) . powerName) roomPowers
  where
    Room _ roomPowers = currentRoom game
    powerName (Power name _ _) = name

runAction :: Game -> Action -> Writer String Game
runAction game command = 
  case command of
       Print string -> do
         tell string
         return game
       GainPower name dispstring -> do
         tell $ replace "_" name dispstring
         let power = getPowerWithName name game
         return $ game {powers = power : powers game}
       LosePower name dispstring -> do
         tell $ replace "_" name dispstring
         let hasName (Power powname _ _) = powname == name 
         return $ game {powers = filter (not . hasName) $ powers game}
       ChooseByCount name strlist -> do
         let count = Map.findWithDefault 0 name $ commandCounts game
         tell $ (concat $ repeat strlist) !! count
         return game
       MoveToRoom name ->
         case Map.lookup name $ rooms game of
              Nothing -> error $ concat ["No room named ", name, " in room list!"]
              Just room@(Room enterActions _) -> do
                let game' = game { currentRoom = room }
                foldM runAction game' enterActions




