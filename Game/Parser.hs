-- | Parse room files, which are written in a custom language for adventure gaming.
module Game.Parser (
  parseRoom
  ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.String.Utils (replace)
import Text.Parsec
import Text.Parsec.String (Parser)

import Game.Types

-- | Parse a room from the contents and filename.
parseRoom :: String -> String -> (RoomName, Room)
parseRoom contents name = 
  case parse room name contents of
       Left err -> error $ show err
       Right parsed -> (replace ".room" "" name, parsed)

-- | Parser for an entire room.
room :: Parser Room
room = do
  -- Parse the enter section, which is required.
  enters <- namedActions "enter"
  whitespace

  -- Parse the optional exit section.
  maybeExit <- optionMaybe $ try $ namedActions "exit"
  whitespace

  -- Parse all the power declarations.
  pows <- many $ try powerDeclaration

  -- After power declarations, we're done.
  whitespace
  eof

  let exitActs = fromMaybe [] maybeExit
  return Room {enterActions = enters, exitActions = exitActs, powerDefinitions = pows}

-- | Parse another parser surrounded by braces and whitespace.
braced :: Parser a -> Parser a
braced parser = do
  -- Parse opening brace and whitespace.
  whitespace
  char '{'
  whitespace

  -- Parse the actual values.
  val <- parser

  -- Parse closing brace and whitespace.
  whitespace
  char '}'
  whitespace
  
  return val

-- | Parse a group of actions with a name, such as the enter or exit actions.
-- | These are just the name followed by a braced set of actions.
namedActions :: String -> Parser [Action]
namedActions name = do
  string name
  braced $ many $ try action

-- | Parse a power declaration.
powerDeclaration :: Parser Power
powerDeclaration = do
  whitespace
  (name, args) <- powerSpec
  actions <- braced $ many1 $ try action
  return $ Power name args actions

-- | Parse one action.
action :: Parser Action
action = whitespace >> choice (map try actionParsers)
  where
    -- All possible actions.
    actionParsers = [respondParser, gainParser, loseParser, 
                     moveToParser, chooseByCountParser, gainItemParser,
                     loseItemParser, ifPossessingParser, synonymParser]

synonymParser :: Parser Action
synonymParser = do
  parts <- actionHeader "synonym"
  void actionString
  return $ PowerTrigger $ unwords parts

gainParser :: Parser Action
gainParser = do
  name : [] <- actionHeader "gain"
  val <- actionString
  return $ GainPower name val

loseParser :: Parser Action
loseParser = do
  name : [] <- actionHeader "lose"
  val <- actionString
  return $ LosePower name val

moveToParser :: Parser Action
moveToParser = do
  name : [] <- actionHeader "move-to"
  void actionString
  return $ MoveToRoom name

chooseByCountParser :: Parser Action
chooseByCountParser = do
  name : [] <- actionHeader "choose-by-count"
  responseChoices <- many1 actionString
  return $ ChooseByCount name responseChoices

respondParser :: Parser Action
respondParser = do
  actionHeader "respond"
  val <- actionString
  return $ Print val

gainItemParser :: Parser Action
gainItemParser = do
  name : [] <- actionHeader "gain-item"
  val <- actionString
  return $ GainItem name val

loseItemParser :: Parser Action
loseItemParser = do
  name : [] <- actionHeader "lose-item"
  val <- actionString
  return $ LoseItem name val

ifPossessingParser :: Parser Action
ifPossessingParser = do
  name : [] <- actionHeader "if-item"
  thenActs <- braced $ many1 $ try action
  elseActs <- braced $ many1 $ try action
  return $ IfPosessingItem name thenActs elseActs

-- | Parse a long string used by an action.
actionString ::  Parser String
actionString = do
  -- Get rid of whitespace.
  whitespace

  -- A semicolon means an empty string, so
  -- try to parse a semicolon first.
  maybeSemicolon <- optionMaybe $ char ';'
  case maybeSemicolon of
    -- If we succeed at parsing a semicolon, return the empty string.
    Just _ -> return ""

    -- If there is no semicolon, then we read everything between an open
    -- and close brace and use that as the string.
    Nothing -> do
      -- Read string.
      str <- braced $ many $ noneOf "}"

      -- Remove indentation by splitting into lines and words and then undoing.
      return $ unlines $ map (unwords . words) $ lines str

-- | Parse an action header. The action header is the bit at the top of an
-- | action that specifies the action and its arguments.
actionHeader :: String -> Parser [String]
actionHeader name = do
  -- The header starts with the name of the action.
  string name

  -- Return all identifiers in the action header.
  -- We need the 'try' because the 'identifier' parser can start
  -- by parsing some whitespace, and then failing, as in
  --   action-name "Some quote" {
  -- In this case, we want it to not consume the whitespace and just fail "immediately".
  many $ try identifier

  where
  -- Define a custom parser for the identifiers.
    identifier = do
      -- Try parsing an open quote.
      whitespace
      maybeOpenQuote <- optionMaybe $ char '"'
      case maybeOpenQuote of
        -- If we can parse an open quote, then the identifier is quote-delimited.
        Just _ -> do
          -- Parse everything until the close quote.
          -- Escape sequences are not included.
          val <- many1 $ noneOf "\""
          char '"'
          return val

        -- If we can't parse an open quote, this is just a space delimited identifier.
        Nothing -> many1 $ noneOf " {;"

-- | Parse the spec for a power.
powerSpec :: Parser (PowerName, [PowerArg])
powerSpec = do
  -- The spec starts with the keyword "power".
  string "power"

  -- Then, it is followed by a list of words, until the opening brace.
  nameAndArgs <- many $ noneOf "{"

  -- Break up the words into the name and the arguments.
  let name : args = words nameAndArgs
  return (name, args)

-- | Parse whitespace, returning nothing.
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t\n"
