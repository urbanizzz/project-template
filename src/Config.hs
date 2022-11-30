{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getLoggerConfig
  , getProjectConfig
  )
where

import qualified ConfigurationTypes as CTypes
import qualified Logger.Impl
import Logger (Level (..))
import qualified System.IO
import qualified Project

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  config <- CTypes.configFromYaml
  case config of
    (Left msg) -> error $ ("From getLoggerConfig: " ++ msg)
    (Right (CTypes.Config logger _)) -> do
      logHandle <- getLogHandler $ CTypes.confPath $ logger
      pure $ Logger.Impl.Config
        { Logger.Impl.confFileHandle  = logHandle
        , Logger.Impl.confMinLevel    = getMinLevel $ CTypes.confLevel $ logger
        }

getLogHandler :: System.IO.FilePath -> IO System.IO.Handle
getLogHandler "stderr" = pure System.IO.stderr
getLogHandler "stdout" = pure System.IO.stdout
getLogHandler file = do
  h <- System.IO.openFile file System.IO.AppendMode
  System.IO.hSetBuffering h System.IO.LineBuffering
  pure h

getMinLevel :: CTypes.LogLevel -> Level
getMinLevel CTypes.Debug    = Debug
getMinLevel CTypes.Info     = Info
getMinLevel CTypes.Warning  = Warning
getMinLevel CTypes.Error    = Error

getProjectConfig :: IO Project.Config
getProjectConfig = do
  config <- CTypes.configFromYaml
  pure $ either (\msg -> error $ ("From getProjectConfig: " ++ msg)) (extractProjectConfig) $ config
  where
    extractProjectConfig (CTypes.Config _ project) = Project.Config
      { Project.field = CTypes.confField project
      }

