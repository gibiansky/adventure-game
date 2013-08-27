{-# LANGUAGE OverloadedStrings #-}
module Game.Actions (
  runCommand,
  getHistory,
  getItems,
  initGame
  ) where

import Control.Monad (foldM)
import Control.Monad.State (State, modify, gets)
import Control.Monad.Writer (runWriter, Writer, tell)
import Data.Aeson (encode, decode)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.String.Utils (replace)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Lazy

import Game.Types

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

getItems :: Lazy.ByteString -> State Game Lazy.ByteString
getItems _ = do
  myItems <- gets items
  return $ encode myItems

noSuchCommand :: CommandResponse
noSuchCommand = Just "error: no matching command found. enqueue headpat."

initGame :: Map.Map RoomName Room -> Game
initGame roomlist =
  let Room enterActs _ _ = fromJust $ Map.lookup "init" roomlist
      game = Game {
        history = [],
        commandCounts = Map.empty,
        currentRoom = fromJust $ Map.lookup "init" roomlist,
        rooms = roomlist,
        lastId = 1,
        powers = [],
        items = []
      }
      (initializedGame, initOut) = runWriter $ foldM runAction game enterActs in
    initializedGame { history = [Command 0 "start" $ Just initOut]}

setOrModify :: Ord k => a -> (a -> a) -> k -> Map.Map k a -> Map.Map k a
setOrModify val modifier key hashmap =
  case Map.lookup key hashmap of
       Nothing -> Map.insert key val hashmap
       Just oldval -> Map.insert key (modifier oldval) hashmap

run :: Command -> Game -> Game
run (Command _ cmdstr _) game = 
  case find (powerMatches cmdstr) (powerDefinitions $ currentRoom game) :: Maybe Power of
       Nothing -> 
         let errcmd = Command (lastId game) cmdstr noSuchCommand in
           game {history = history game ++ [errcmd], lastId = 1 + lastId game}
       Just pow -> 
         let (game', cmdOutput) = runWriter $ foldM runAction game $ powerActions pow
             newCommand = Command (lastId game) cmdstr $ Just cmdOutput
             newCounts = setOrModify 1 (+1) (powerName pow) $ commandCounts game' 
             game'' = game' {history = history game ++ [newCommand], lastId = 1 + lastId game, commandCounts = newCounts} in
          game''

powerMatches :: PowerName -> Power -> Bool
powerMatches str Power { powerName = name, powerArgs = args } =
  words str == name : args

runAction :: Game -> Action -> Writer String Game
runAction game command = 
  case command of
       Print string -> do
         tell string
         return game
       GainPower name dispstring -> do
         tell $ replace "_" name dispstring
         return $ game {powers = name : powers game}
       LosePower name dispstring -> do
         tell $ replace "_" name dispstring
         return $ game {powers = filter (/= name) $ powers game}
       ChooseByCount name strlist -> do
         let ct = Map.findWithDefault 0 name $ commandCounts game
         tell $ cycle strlist !! ct
         return game
       MoveToRoom name ->
         let actions = exitActions $ currentRoom game in
           case Map.lookup name $ rooms game of
             Nothing -> error $ concat ["No room named ", name, " in room list!"]
             Just room -> do
               let game' = game { currentRoom = room, commandCounts = Map.empty }
               foldM runAction game' (actions ++ enterActions room)
       GainItem itemName dispstring -> do
         tell dispstring
         return game {items = itemName : items game}
       LoseItem itemName dispstring -> do
         tell dispstring
         return game {items = filter (/= itemName) $ items game}
       IfPosessingItem itemName thenActs elseActs ->
         foldM runAction game (if itemName `elem` items game then thenActs else elseActs)
       PowerTrigger cmdstr ->
         let pow = fromJust $ find (powerMatches cmdstr) (powerDefinitions $ currentRoom game) in
           foldM runAction game (powerActions pow)
