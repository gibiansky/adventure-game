{-# LANGUAGE TypeFamilies #-}
module Game.Types where

import qualified Data.Map as Map

data Game = Game {
  history :: [Command],
  commandCounts :: Map.Map PowerName Int,
  currentRoom :: Room,
  rooms :: Map.Map String Room,
  powers :: [PowerName],
  lastId :: Int,
  items :: [ItemName]
  } deriving Show
type CommandId = Int  
type CommandResponse = Maybe String
data Command = Command CommandId String CommandResponse deriving Show


data Room = Room {
  enterActions :: [Action],
  exitActions :: [Action],
  powerDefinitions :: [Power]
  } deriving Show
data Action = Print String |
              GainPower PowerName String |
              LosePower PowerName String |
              ChooseByCount PowerName [String] |
              MoveToRoom String |
              GainItem ItemName String |
              LoseItem ItemName String |
              IfPosessingItem String [Action] [Action] |
              PowerTrigger String deriving Show

type ItemName = String
type EventName = String

type PowerName = String
type PowerArg = String
data Power = Power PowerName [PowerArg] [Action]
instance Eq Power where
  Power powname1 powargs1 _ == Power powname2 powargs2 _ = 
    (powargs1 == powargs2) && powname1 == powname2

type RoomName = String

instance Show Power where
  show (Power name args actions) = "<power: " ++ unwords (name : args) ++ ">\n" ++ actionSection 
    where actionSection = unlines $ map ("    " ++) $ lines . unlines $ map show actions

