module Game.Actions where

import Game.Types

import Control.Monad.State
import Data.ByteString
import Control.Applicative
import Data.Aeson

import Data.String.Utils
import qualified Data.Map as Map

noSuchCommand :: String
noSuchCommand = "error: no matchinc command found. enqueue headpat."

instance ToJSON Command where
  toJSON (Command id cmd Nothing) = toJSON $ Command id cmd $ Just "No response."
  toJSON (Command id cmd (Just response)) = object ["id" .= id, "command" .= cmd, "response" .= response]

instance FromJSON Command where
  fromJSON (Object v) = Command <$> v .: "id" <*> v .: "command" <*> Nothing
  fromJSON _ = mzero

initGame :: Map.Map String Room -> Game
initGame rooms =
  let initRoom@(Room enterActions _) = fromJust $ Map.lookup "init" rooms
      game = Game {
        history = [],
        commandCounts = Map.empty,
        currentRoom = fromJust $ Map.lookup "init" rooms,
        rooms = rooms
      }
      (initOut, initializedGame) = runWriter $ foldM runAction game enterActions in
    initializedGame { history = [Command 0 "start" $ Just initOut]}


runCommand :: ByteString -> State Game ByteString
runCommand commandStr = 
  let command = decode commandStr in
      do
        modify $ run command
        gets (encode . last . history)

getHistory :: ByteString -> State Game ByteString
getHistory _ = do
  commands <- gets history
  return $ encode commands

run :: Command -> Game -> Game
run command game = 
  case find (powerMatches command) (powers game) of
       Nothing -> command { id = lastId game, response = noSuchCommand }
       Just (Power _ _ actions) -> 
         let (cmdOutput, game') = runWriter $ foldM runAction game actions
             newCommand = command { id = lastId game, response = cmdOutput }
             game'' = game' {history = history game ++ [newCommand]} in
          game

powerMatches :: Command -> Power -> Bool
powerMatches (Command _ str _) (Power name args _) =
  words str == name : args

getPowerWithName :: String -> Game -> Power
getPowerWithName name game = find ((name ==) . powerName) roomPowers
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
         return $ game {powers = filter (/= name) $ powers game}
       ChooseByCount name strlist -> do
         let count = Map.findWithDefault 0 name $ commandCounts game
         tell $ (concat.repeat strlist) !! count
         return game
       MoveToRoom name ->
         case Map.lookup name $ rooms game of
              Nothing -> error $ concat ["No room named ", name, " in room list!"]
              Maybe room@(Room enterActions _) -> do
                let game' = game { currentRoom = room }
                forM_ runAction enterActions
                return game'




