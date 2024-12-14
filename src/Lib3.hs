{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lib3
  ( stateTransition,
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    renderQuery,
    StorageOp(..),
    Statements(..),
    Command(..),
  )
where

import Control.Applicative (Alternative(many), (<|>))
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Maybe (fromJust, isNothing)
import System.Directory (doesFileExist)
import qualified Lib2
import qualified Parsers

-- | Storage Operation data type to represent Save and Load operations
data StorageOp = Save String (Chan ()) | Load (Chan (Maybe String))

-- | This function is started from main
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile "state.txt" s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist "state.txt"
      if exists
        then do
          s' <- readFile "state.txt"
          writeChan chan $ Just s'
        else writeChan chan Nothing

-- | Statements data type to represent a batch or single command
data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Eq, Show)

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

-- Parsing Functions

parseCommand :: String -> Either String (Command, String)
parseCommand = Parsers.runParser (parseLoad <|> parseSave <|> (StatementCommand <$> parseStatements))

parseLoad :: Parsers.Parser Command
parseLoad = LoadCommand <$ (Parsers.skipSpaces *> Parsers.parseLiteral "load")

parseSave :: Parsers.Parser Command
parseSave = SaveCommand <$ (Parsers.skipSpaces *> Parsers.parseLiteral "save")

parseStatements :: Parsers.Parser Statements
parseStatements = parseBatch <|> (Single <$> Parsers.parseStatement)

parseBatch :: Parsers.Parser Statements
parseBatch = do
  Parsers.skipSpaces
  Parsers.parseLiteral "BEGIN"
  Parsers.skipSpaces
  qs <- many (Parsers.parseStatement <* Parsers.parseLiteral ";" <* Parsers.skipSpaces)
  Parsers.parseLiteral "END"
  return $ Batch qs

-- | Converts program's state into Statements
marshallState :: Lib2.State -> Statements
marshallState state = Batch queries
  where
    userQueries = map Lib2.RegisterUserStmt (Lib2.users state)
    bikeQueries = map Lib2.RegisterBikeStmt (Lib2.bikes state)
    rentQueries = map (\(bid, uid) -> Lib2.RentStmt uid bid) (Lib2.rentedBikes state)
    queries = userQueries ++ bikeQueries ++ rentQueries

-- Rendering Functions
renderQuery :: Lib2.Query -> String
renderQuery (Lib2.RentStmt uid bid) = "rent " ++ Lib2.showUserID uid ++ " " ++ Lib2.showBikeID bid
renderQuery (Lib2.ReturnStmt bid uid) = "return " ++ Lib2.showBikeID bid ++ " " ++ Lib2.showUserID uid
renderQuery (Lib2.CheckBikeStmt bid) = "check bike " ++ Lib2.showBikeID bid
renderQuery (Lib2.CheckUserStmt uid) = "check user " ++ Lib2.showUserID uid
renderQuery (Lib2.RegisterBikeStmt bid) = "register bike " ++ Lib2.showBikeID bid
renderQuery (Lib2.RegisterUserStmt uid) = "register user " ++ Lib2.showUserID uid
renderQuery Lib2.ExitStmt = "exit"
renderQuery (Lib2.Sequence queries) = intercalate "; " (map renderQuery queries)

renderStatements :: Statements -> String
renderStatements (Single q) = renderQuery q
renderStatements (Batch qs) = "BEGIN\n" ++ concatMap (\q -> renderQuery q ++ ";\n") qs ++ "END\n"

-- | Updates a state according to a command.
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  let content = renderStatements $ marshallState s'
  chan <- newChan
  writeChan ioChan (Save content chan)
  readChan chan
  return $ Right $ Just "State saved successfully."
stateTransition s LoadCommand ioChan = do
  chan <- newChan
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if isNothing qs
    then return (Left "No state file found.")
    else case Parsers.runParser parseStatements (fromJust qs) of
      Left e -> return $ Left $ "Failed to load state from file:\n" ++ e
      Right (stmts, _) -> atomically $ atomicStatements s stmts
stateTransition s (StatementCommand stmts) _ = atomically $ atomicStatements s stmts

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s (Batch qs) = do
  s' <- readTVar s
  case processQueries s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
atomicStatements s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg

processQueries :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
processQueries st [] = Right (Nothing, st)
processQueries st (q:qs) =
  case Lib2.stateTransition st q of
    Left err -> Left err
    Right (msg1, st1) ->
      case processQueries st1 qs of
        Left err -> Left err
        Right (msg2, st2) ->
          Right (combineMessages msg1 msg2, st2)

combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing m = m
combineMessages m Nothing = m
combineMessages (Just m1) (Just m2) = Just (m1 ++ "\n" ++ m2)
