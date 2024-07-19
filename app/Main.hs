module Main where

import Data.Version (Version, showVersion)
import Options.Applicative
import Paths_1brc qualified as Meta
import RIO
import RIO.PrettyPrint.StylesUpdate
import RIO.Process (mkDefaultProcessContext)
import Run (run)
import Types

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) parseOpts
  logOpts <- logOptionsHandle stderr $ aoDebug opts
  processCtx <- mkDefaultProcessContext
  withLogFunc logOpts $ \logFn ->
    let app =
          App
            { appLogFn = logFn,
              appOptions = opts,
              appProcessContext = processCtx,
              appUseColor = True,
              appTermWidth = 80,
              appStylesUpdate = StylesUpdate []
            }
     in runRIO app run

parseOpts :: ParserInfo AppOptions
parseOpts =
  info (helper <*> parseVersion <*> programOptions)
    $ fullDesc
    <> header "1 Billion Row Challenge"
    <> progDesc "Run the current aggregation implementation optimized for speed."
    <> footer "For more information, please visit https://1brc.dev"

parseVersion :: Parser (a -> a)
parseVersion =
  infoOption
    (prettyVersion Meta.version)
    (long "version" <> help "Show version")
  where
    prettyVersion :: Version -> [Char]
    prettyVersion = (++) "1brc v" . showVersion

programOptions :: Parser AppOptions
programOptions =
  AppOptions
    <$> parseDebug
    <*> parseInputFilePath
    <*> parseOutputFilePath

parseDebug :: Parser Bool
parseDebug =
  switch
    $ long "debug"
    <> short 'd'
    <> help "Output information useful for debugging"

parseInputFilePath :: Parser FilePath
parseInputFilePath =
  strOption
    $ long "file"
    <> short 'f'
    <> metavar "FILE_PATH"
    <> help "Path to measurements file"

parseOutputFilePath :: Parser (Maybe FilePath)
parseOutputFilePath =
  optional
    $ strOption
    $ long "output"
    <> short 'o'
    <> metavar "FILE_PATH"
    <> help "Path to output calculations. Output will default to stdio if not provided."
