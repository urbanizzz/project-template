{-# LANGUAGE OverloadedStrings #-}

module ConfigurationTypes
  ( Config (..)
  , LoggerConfig (..)
  , LogLevel (..)
  , ProjectConfig (..)
  , configFromYaml
  )
where

import Data.Yaml ((.:))
import qualified Data.Yaml as Yaml 
import qualified System.IO
import Data.Text (Text)

configFile :: System.IO.FilePath
configFile = "./config.yaml"

data Config = Config
  { confLogger        :: LoggerConfig
  , confProjectConfig :: ProjectConfig
  }

instance Yaml.FromJSON Config where
  parseJSON = Yaml.withObject "FromJSON Config" $ \o -> Config
    <$> o .: "logger"
    <*> o .: "projectConfig"

data LoggerConfig = LoggerConfig
  { confPath  :: FilePath
  , confLevel :: LogLevel
  }

instance Yaml.FromJSON LoggerConfig where
  parseJSON = Yaml.withObject "FromJSON LoggerConfig" $ \o -> LoggerConfig
    <$> o .: "path"
    <*> o .: "logLevel"

data LogLevel
  = Debug
  | Info
  | Warning
  | Error

instance Yaml.FromJSON LogLevel where
  parseJSON = Yaml.withText "FromJSON LogLevel" $ \t ->
    case t of
      "debug"   -> pure Debug
      "info"    -> pure Info
      "warning" -> pure Warning
      "error"   -> pure Error
      _         -> pure Error

-- | Project config example
data ProjectConfig = ProjectConfig
  { confField :: Text
  }

instance Yaml.FromJSON ProjectConfig where
  parseJSON = Yaml.withObject "FromJSON ProjectConfig" $ \o -> ProjectConfig
    <$> o .: "field"

configFromYaml :: IO (Either String Config)
configFromYaml = do
  config <- Yaml.decodeFileEither configFile
  pure $ either (Left . Yaml.prettyPrintParseException) (Right) $ config

