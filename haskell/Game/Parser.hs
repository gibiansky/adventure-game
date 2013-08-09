module Game.Parser where

import Game.Types

import Text.Parsec
import Text.Parsec.String

import Control.Monad

import Debug.Trace
import Data.String.Utils

parseRoom :: String -> String -> (String, Room)
parseRoom contents name = 
  case parse room name contents of
       Left err -> error $ show err
       Right parsed -> (replace ".room" "" name, parsed)

room :: Parser Room
room = do
  section $ string "enter"
  whitespace
  enterCmds <- many action
  powers <- many $ try powerDeclaration
  whitespace
  eof
  return $ Room enterCmds powers

action :: Parser Action
action = whitespace >> choice actionParsers
  where
    actionParsers = map try [respondParser, gainParser, loseParser, moveToParser, chooseByCountParser]

powerDeclaration :: Parser Power
powerDeclaration = do
  whitespace
  (name, args) <- powerSpec
  actions <- many1 $ try action
  return $ Power name args actions

gainParser = do
  name : [] <- actionSpec "gain" 1
  val <- stringAction
  return $ GainPower name val

loseParser = do
  name : [] <- actionSpec "lose" 1
  val <- stringAction
  return $ LosePower name val

moveToParser = do
  name : [] <- actionSpec "move-to" 1
  return $ MoveToRoom name

chooseByCountParser = do
  name : [] <- actionSpec "choose-by-count" 1
  eol
  responseChoices <- many1 $ try responseOptionAction
  return $ ChooseByCount name responseChoices

respondParser :: Parser Action
respondParser = do
  actionSpec "respond" 0
  val <- stringAction
  return $ Print val

responseOptionAction = do
  between whitespace whitespace $ string "{"
  str <- manyTill anyChar $ lookAhead $ string "}"
  string "}"
  return $ unlines $ map (unwords . words) $ lines str

stringAction :: Parser String
stringAction = do
  str <- many $ noneOf "[<"
  return $ unlines $ map (unwords . words) $ lines str

nothingAction :: Parser ()
nothingAction = void whitespace

actionSpec :: String -> Int -> Parser [String]
actionSpec name nargs = surround '[' ']' spec
  where
    spec = do
      string name
      if nargs == 0
      then return []
      else do
        char ':'
        args <- many $ noneOf "]"
        return $ words args

powerSpec :: Parser (PowerName, [PowerArg])
powerSpec = do
  string "<power:"
  nameAndArgs <- many $ noneOf ">"
  char '>'
  let name : args = words nameAndArgs
  return (name, args)

section :: Parser a -> Parser a
section = surround '<' '>'

command :: Parser a -> Parser a
command = surround '<' '>'

surround :: Char -> Char -> Parser a -> Parser a
surround startChar endChar = between (char startChar) (char endChar)

whitespace :: Parser String
whitespace = many $ oneOf " \t\n"

eol :: Parser ()
eol = void $ string "\n"
