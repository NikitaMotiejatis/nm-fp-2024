{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import qualified Lib2
import qualified Lib3
import Web.Scotty

-- Main entry point for the server
main :: IO ()
main = do
  -- Create a shared channel for storage operations
  storageChan <- newChan :: IO (Chan Lib3.StorageOp)

  -- Create a shared TVar for state management
  state <- newTVarIO Lib2.emptyState

  -- Start the storage operation loop in a separate thread
  _ <- forkIO $ Lib3.storageOpLoop storageChan

  -- Start the Scotty web server
  scotty 3000 $ do
    post "/" $ do
      -- Read and log the request body
      requestBody <- body
      liftIO $ putStrLn $ "Request received: " ++ cs requestBody

      -- Process the request and generate a response
      response <- liftIO $ handleRequest state storageChan $ cs requestBody

      -- Return the response
      text $ cs response

-- Handle incoming requests by parsing and executing commands
handleRequest :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
handleRequest state storageChan input =
  case Lib3.parseCommand input of
    Left err -> return $ "PARSE ERROR: " ++ err
    Right (command, "") -> do
      result <- Lib3.stateTransition state command storageChan
      case result of
        Left err -> return $ "EXECUTION ERROR: " ++ err
        Right output -> do
          putStrLn $ "Server response: " ++ fromMaybe "Success" output
          return $ fromMaybe "Success" output
    Right (_, remaining) -> return $ "PARSE ERROR: Unexpected input: " ++ remaining

-- Example: parseCommand and stateTransition functions should be implemented in `Lib3` based on your current state.
