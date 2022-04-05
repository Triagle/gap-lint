module Main where

import AST
import Control.Monad (when)
import Data.Either (isRight)
import Data.List (intercalate, nub)
import Options.Applicative
import Text.Parsec (eof, sourceName, unexpected)
import Text.Parsec.Error (Message, ParseError)
import Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.String (parseFromFile)
import Text.ParserCombinators.Parsec.Error (Message (SysUnExpect), errorMessages)

data Options = Options
  { filePath :: FilePath
  }

formatMessage :: Message -> String
formatMessage (SysUnExpect "") = "unexpected end of input"
formatMessage (SysUnExpect msg) = "unexpected " <> msg
formatMessage (UnExpect msg) = "unexpected " <> msg
formatMessage (Expect msg) = "expected " <> msg
formatMessage (Message msg) = msg

replaceNl :: String -> String
replaceNl ('\n' : xs) = " " <> replaceNl xs
replaceNl (x : xs) = x : replaceNl xs
replaceNl "" = ""

cli :: ParserInfo Options
cli = info (Options <$> argument str (metavar "FILE")) (fullDesc <> progDesc "Check GAP files for syntax errors.")

main :: IO ()
main = do
  opts <- execParser cli
  res <- parseFromFile (statements <* eof) (filePath opts)
  case res of
    Left err -> do
      putStrLn . replaceNl . show $ err
    Right () -> return ()
