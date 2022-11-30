{-# LANGUAGE OverloadedStrings #-}

module Project
  ( Config (..)
  , Handle (..)
  , run
  )
where

import qualified Data.Text as T
import Logger ((.<))
import qualified Logger

data Config = Config
  { field :: T.Text
  }

data Handle m = Handle
  { hLogHandle :: Logger.Handle m
  , hConfig :: Config
  }

run :: Handle IO -> IO ()
run h = do
  Logger.logInfo (hLogHandle h) "Application is runned"
  putStrLn "Application is successfully runned"
  putStr "Config: field="
  putStrLn (T.unpack . field . hConfig $ h)

