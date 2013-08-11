{-# LANGUAGE TypeFamilies #-}
module Game.Types where

import qualified Data.Map as Map

data Game = Game {
  history :: [Command],
  commandCounts :: Map.Map PowerName Int,
  currentRoom :: Room,
  rooms :: Map.Map String Room,
  powers :: [Power],
  lastId :: Int
  } deriving Show
type CommandId = Int  
type CommandResponse = Maybe String
data Command = Command CommandId String CommandResponse deriving Show


data Room = Room {
  enterActions :: [Action],
  exitActions :: [Action],
  powerDefinitions :: [Power]
  }
data Action = Print String |
              GainPower PowerName String |
              LosePower PowerName String |
              ChooseByCount PowerName [String] |
              MoveToRoom String 

type PowerName = String
type PowerArg = String
data Power = Power PowerName [PowerArg] [Action]

type RoomName = String

-- Debug instances
instance Show Room where
  show room = 
    let header = "\nRoom:\n-----\n<enter>\n"
        enterSection = unlines $ map ("    " ++) $ lines . unlines $ map show $ enterActions room
        powerSection = unlines $ map show $ powerDefinitions room in
      header ++ enterSection ++ powerSection

instance Show Action where
  show (Print str) = "[respond]\n" ++ str
  show (GainPower name str) = "[gain: " ++ name ++ "]\n" ++ str
  show (LosePower name str) = "[gain: " ++ name ++ "]\n" ++ str
  show (ChooseByCount name strs) = "[choose-by-count: " ++ name ++ "]\n" ++ options
    where 
      bracewrap val = "{\n" ++ val ++ "}\n"
      options = unlines $ map bracewrap strs
  show (MoveToRoom name) = "[move-to: " ++ name ++ "]"

instance Show Power where
  show (Power name args actions) = "<power: " ++ unwords (name : args) ++ ">\n" ++ actionSection 
    where actionSection = unlines $ map ("    " ++) $ lines . unlines $ map show actions

