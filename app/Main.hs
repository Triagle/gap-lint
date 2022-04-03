module Main where

import AST
import Control.Monad (when)
import Data.Either (isRight)
import Data.List (intercalate)
import Options.Applicative
import Text.Parsec (eof, unexpected)
import Text.Parsec.Error as PE
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.String (parseFromFile)

data Options = Options
  { filePath :: FilePath
  }

cli :: ParserInfo Options
cli = info (Options <$> argument str (metavar "FILE")) (fullDesc <> progDesc "Check GAP files for syntax errors.")

-- printParsecError :: PE.ParseError -> IO ()
-- printParsecError err = print

-- where
--   pos = errorPos err
--   line = sourceLine pos
--   col = sourceColumn pos
--   name = sourceName pos
--   message = showErrorMessages "or" "unkown parse error" "expecting" "unexpected String" String ([Message])

main :: IO ()
main = do
  opts <- execParser cli
  res <- parseFromFile (statements <* eof) (filePath opts)
  case res of
    Left err -> print err
    Right () -> return ()
