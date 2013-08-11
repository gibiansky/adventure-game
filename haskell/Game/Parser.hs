module Game.Parser where

import Game.Types

import Text.Parsec
import Text.Parsec.String

import Control.Monad

import Data.String.Utils
import Data.Maybe

parseRoom :: String -> String -> (String, Room)
parseRoom contents name = 
  case parse room name contents of
       Left err -> error $ show err
       Right parsed -> (replace ".room" "" name, parsed)

room :: Parser Room
room = do
  enters <- namedActions "enter"
  whitespace
  maybeExit <- optionMaybe $ try $ namedActions "exit"
  whitespace
  pows <- many $ try powerDeclaration
  whitespace
  eof
  let exitActs = fromMaybe [] maybeExit
  return Room {enterActions = enters, exitActions = exitActs, powerDefinitions = pows}

braced :: Parser a -> Parser a
braced parser = do
  whitespace
  char '{'
  whitespace
  val <- parser
  whitespace
  char '}'
  whitespace
  return val

namedActions :: String -> Parser [Action]
namedActions name = do
  string name
  braced $ many action

powerDeclaration :: Parser Power
powerDeclaration = do
  whitespace
  (name, args) <- powerSpec
  actions <- braced $ many1 $ try action
  return $ Power name args actions

action :: Parser Action
action = whitespace >> choice actionParsers
  where
    actionParsers = map try [respondParser, gainParser, loseParser, moveToParser, chooseByCountParser]

gainParser :: Parser Action
gainParser = do
  name : [] <- actionSpec "gain"
  val <- stringAction
  return $ GainPower name val

loseParser :: Parser Action
loseParser = do
  name : [] <- actionSpec "lose"
  val <- stringAction
  return $ LosePower name val

moveToParser :: Parser Action
moveToParser = do
  name : [] <- actionSpec "move-to"
  void stringAction
  return $ MoveToRoom name

chooseByCountParser :: Parser Action
chooseByCountParser = do
  name : [] <- actionSpec "choose-by-count"
  responseChoices <- many1 stringAction
  return $ ChooseByCount name responseChoices

respondParser :: Parser Action
respondParser = do
  actionSpec "respond"
  val <- stringAction
  return $ Print val

stringAction ::  Parser String
stringAction = do
  str <- braced $ many $ noneOf "}"
  return $ unlines $ map (unwords . words) $ lines str

actionSpec :: String -> Parser [String]
actionSpec name = do
  string name
  args <- many $ noneOf "{"
  return $ words args

powerSpec :: Parser (PowerName, [PowerArg])
powerSpec = do
  string "power"
  nameAndArgs <- many $ noneOf "{"
  let name : args = words nameAndArgs
  return (name, args)

whitespace :: Parser String
whitespace = many $ oneOf " \t\n"

eol :: Parser ()
eol = void $ string "\n"
