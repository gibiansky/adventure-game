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
    actionParsers = map try [respondParser, gainParser, loseParser, moveToParser, chooseByCountParser, gainItemParser, loseItemParser, ifPossessingParser, eventParser]

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

eventParser :: Parser Action
eventParser = do
  name : [] <- actionSpec "event"
  void stringAction
  return $ Event name

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

gainItemParser :: Parser Action
gainItemParser = do
  name : [] <- actionSpec "gain-item"
  val <- stringAction
  return $ GainItem name val

loseItemParser :: Parser Action
loseItemParser = do
  name : [] <- actionSpec "lose-item"
  val <- stringAction
  return $ GainItem name val

ifPossessingParser :: Parser Action
ifPossessingParser = do
  name : [] <- actionSpec "if-item"
  thenActs <- braced $ many1 $ try action
  elseActs <- braced $ many1 $ try action
  return $ IfPosessingItem name thenActs elseActs

stringAction ::  Parser String
stringAction = do
  whitespace
  maybeSemicolon <- optionMaybe $ char ';'
  case maybeSemicolon of
    Just _ -> return ""
    Nothing -> do
      str <- braced $ many $ noneOf "}"
      return $ unlines $ map (unwords . words) $ lines str

actionSpec :: String -> Parser [String]
actionSpec name = do
  string name

  let identifier = do
        whitespace
        maybeOpenQuote <- optionMaybe $ char '"'
        case maybeOpenQuote of
          Just _ -> do
            val <- many1 $ noneOf "\""
            char '"'
            return val
          Nothing -> many1 $ noneOf " {;"
  many $ try identifier

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
