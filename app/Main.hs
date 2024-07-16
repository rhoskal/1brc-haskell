{-# LANGUAGE TemplateHaskell #-}

module Main where

import MyLib (runV0)
import Options.Applicative.Simple
import Paths_1brc qualified
import RIO
import RIO.Process
import Types

main :: IO ()
main = do
  (options, ()) <- optsParser
  logOpts <- logOptionsHandle stderr (optionsVerbose options)
  processCtx <- mkDefaultProcessContext
  withLogFunc logOpts $ \logFn ->
    let app =
          App
            { appLogFn = logFn,
              appOptions = options,
              appProcessContext = processCtx
            }
     in runRIO app runV0

optsParser :: IO (Options, ())
optsParser = do
  simpleOptions
    $(simpleVersion Paths_1brc.version)
    "Header for command line arguments"
    "Run different variations of the 1brc"
    (Options <$> switch (long "verbose" <> short 'v' <> help "Verbose output?"))
    empty
