{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Version qualified as V
import MyLib (runV0)
import Options.Applicative.Simple
import Paths_1brc qualified as Meta
import RIO
import RIO.Process (mkDefaultProcessContext)
import Types

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) optsParser
  logOpts <- logOptionsHandle stderr $ aoDebug opts
  processCtx <- mkDefaultProcessContext
  withLogFunc logOpts $ \logFn ->
    let app =
          App
            { appLogFn = logFn,
              appOptions = opts,
              appProcessContext = processCtx
            }
     in runRIO app runV0

optsParser :: ParserInfo AppOptions
optsParser =
  info (helper <*> versionParser <*> programOptions)
    $ fullDesc
    <> header "1 Billion Row Challenge"
    <> progDesc "Run different variations of the 1brc"
    <> footer "For more information, please visit https://1brc.dev"

versionParser :: Parser (a -> a)
versionParser =
  infoOption
    (prettyVersion Meta.version)
    (long "version" <> help "Show version")
  where
    prettyVersion :: V.Version -> [Char]
    prettyVersion = (++) "1brc v" . V.showVersion

programOptions :: Parser AppOptions
programOptions =
  AppOptions
    <$> debugParser

debugParser :: Parser Bool
debugParser =
  switch
    $ long "debug"
    <> short 'd'
    <> help "Output information useful for debugging"
