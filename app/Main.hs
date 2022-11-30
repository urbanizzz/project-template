module Main
  ( main,
  )
where

import qualified Config
import qualified Logger
import qualified Logger.Impl
import qualified Project

main :: IO ()
main = do
  withLogHandle $ \logHandle -> do
    config <- Config.getProjectConfig
    Project.run $ Project.Handle 
      { Project.hLogHandle = logHandle
      , Project.hConfig = config
      }

withLogHandle :: (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle f = do
  config <- Config.getLoggerConfig
  Logger.Impl.withHandle config f

{-
makeBotHandleForPlainText :: Logger.Handle IO -> IO (EchoBot.Handle IO T.Text)
makeBotHandleForPlainText logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  stateRef <- newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef stateRef
      , EchoBot.hModifyState' = modifyIORef' stateRef
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      , EchoBot.hTextFromMessage = Just
      , EchoBot.hMessageFromText = id
      }
      -}
