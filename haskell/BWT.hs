module Main where

import System.Environment(getArgs)
import Data.List

printUsage :: IO ()
printUsage = 
  putStrLn $ unlines [
      "Usage: ./bwt <command> [str...]"
    , "\tbwt encode - encode strings using Burrows-Wheeler Transform"
    , "\tbwt decode - decode strings using inverse Burrows-Wheeler Transform"
    ]

main ::  IO ()
main = do
  args <- getArgs
  case args of
    "encode" : datas -> mapM_ (putStrLn . encode . surround) datas
    "decode" : datas -> mapM_ (putStrLn . decode) datas
    _ -> printUsage

surround ::  String -> String
surround str = '^' : str

encode :: String -> String
encode string = transformed
  where shift i = drop i string ++ take i string
        shifts = map shift [0..length string - 1]
        sorted = sort shifts
        transformed = map last sorted

decode :: String -> String
decode string = 
  let emptyTable = replicate (length string) []
      table = iterate insertionStep emptyTable !! length string in
     tail . head $ filter ((== '^') . head) table
  where
    insertionStep table = sort $ zipWith (:) string table
