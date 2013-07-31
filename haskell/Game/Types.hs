{-# LANGUAGE TypeFamilies #-}
module Game.Types where

import qualified Data.Map as Map

data Game = Game {
  history :: [Command],
  currentRoom :: Room,
  commandCounts :: Map.Map PowerName Int
  }
type CommandId = Int  
data CommandResponse = Maybe String
data Command = Command CommandId String CommandResponse


data Room = Room [Action] [Power]
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
  show (Room actions powers) = 
    let header = "\nRoom:\n-----\n<enter>\n"
        enterSection = unlines $ map ("    " ++) $ lines . unlines $ map show actions
        powerSection = unlines $ map show powers in
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

