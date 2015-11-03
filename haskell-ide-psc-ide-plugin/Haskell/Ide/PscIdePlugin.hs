{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.PscIdePlugin where

import           Haskell.Ide.Engine.PluginDescriptor

import           Data.Aeson
import qualified Data.Map                            as Map
import qualified Data.Text                           as T

import           Control.Exception
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text.IO                        as T
import           Network
-- import           Options.Applicative
import           System.Exit
import           System.IO

-- ---------------------------------------------------------------------

pscIdeDescriptor :: PluginDescriptor
pscIdeDescriptor = PluginDescriptor
  {
    pdUiCommands =
      [
        UiCommand
          { uiCmdName = "request"
          , uiContexts = [CtxNone]
          , uiAdditionalParams = [RP "command", RP "port"]
          }
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

-- ---------------------------------------------------------------------

pscIdeDispatcher :: Dispatcher
pscIdeDispatcher (IdeRequest name _ _ params) =
  case name of
    "request" -> do
      let command = Map.lookup "command" params
          port = PortNumber (fromIntegral
             (fromMaybe (4242 :: Integer) (read <$> Map.lookup "port" params)))
      result <- sequence $ sendToPscIde port <$> command
      return (maybe (IdeResponseFail "You failed") (IdeResponseOk . String) result)
    _ -> return $ IdeResponseFail "Unsupported Command"

-- -----------------------------------------------------------------------

sendToPscIde :: PortID -> String -> IO T.Text
sendToPscIde port command = do
    h <-
        connectTo "localhost" port `catch`
        (\(SomeException _) ->
              putStrLn
                  ("Couldn't connect to psc-ide-server on port: " ++
                   show port) >>
              exitFailure)
    hPutStrLn h command
    res <- T.hGetLine h
    hFlush stdout
    hClose h
    return res
