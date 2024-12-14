{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE FlexibleInstances #-}

module Parsers
    ( Parser,
      parseLiteral,
      skipSpaces,
      parseInt,
      parseUserID,
      parseBikeID,
      parseStatement,
      parseQuery,
      runParser  
    ) where

import Debug.Trace (trace)
import Control.Applicative (Alternative(..))
import Control.Monad.Except
import Control.Monad.Except (throwError, catchError)
import Control.Applicative (Alternative(..), optional)
import Control.Monad.State
import Data.Char (isDigit, isSpace, toLower)
import Lib2 (Query(..), UserID(..), BikeID(..))

-- | Parser type definition using ExceptT and State
type Parser a = ExceptT String (State String) a

-- runParser: a helper to run our Parser on an input string
runParser :: Parser a -> String -> Either String (a, String)
runParser p input =
    let (res, remaining) = runState (runExceptT p) input in
    case res of
      Left err -> Left err
      Right val -> Right (val, remaining)

-- Parsing Helpers

skipSpaces :: Parser ()
skipSpaces = do
    input <- lift get
    let rest = dropWhile isSpace input
    lift (put rest)

parseLiteral :: String -> Parser String
parseLiteral s = do
    skipSpaces
    input <- lift get
    trace ("parseLiteral expecting \"" ++ s ++ "\" got: " ++ show (take 20 input) ++ "...") (return ())
    let len = length s
        (prefix, rest) = splitAt len input
    if map toLower prefix == map toLower s
       then do
           lift (put rest)
           return s
       else throwError $ "Expected \"" ++ s ++ "\", but found \"" ++ prefix ++ "\""

parseInt :: Parser Int
parseInt = do
    skipSpaces
    input <- lift get
    let (digits, rest) = span isDigit input
    if null digits
       then throwError "Expected an integer"
       else do lift (put rest)
               return (read digits)

parseUserID :: Parser UserID
parseUserID = do
    trace "Parsing UserID..." (return ())
    _ <- parseLiteral "user"
    uid <- parseInt
    trace ("UserID parsed as " ++ show uid) (return ())
    return (UserID uid)

parseBikeID :: Parser BikeID
parseBikeID = do
    _ <- parseLiteral "bike"
    bid <- parseInt
    return (BikeID bid)

-- Parsers for specific queries
parseRentStmt :: Parser Query
parseRentStmt = do
    _ <- parseLiteral "rent"
    uid <- parseUserID
    bid <- parseBikeID
    return (RentStmt uid bid)

parseReturnStmt :: Parser Query
parseReturnStmt = do
    _ <- parseLiteral "return"
    bid <- parseBikeID
    uid <- parseUserID
    return (ReturnStmt bid uid)

parseCheckStmt :: Parser Query
parseCheckStmt = do
    _ <- parseLiteral "check"
    skipSpaces
    bikeParser <|> userParser
  where
    bikeParser = do
        _ <- parseLiteral "bike"
        bid <- parseBikeID
        return (CheckBikeStmt bid)
    userParser = do
        _ <- parseLiteral "user"
        uid <- parseUserID
        return (CheckUserStmt uid)


parseRegisterStmt :: Parser Query
parseRegisterStmt = do
    _ <- parseLiteral "register"
    skipSpaces
    userParser <|> bikeParser
  where
    userParser = do
        _ <- parseLiteral "user"
        uid <- parseUserID
        trace ("Parsed register user statement: " ++ show uid) $ return ()
        return (RegisterUserStmt uid)
    bikeParser = do
        _ <- parseLiteral "bike"
        bid <- parseBikeID
        trace ("Parsed register bike statement: " ++ show bid) $ return ()
        return (RegisterBikeStmt bid)


parseExitStmt :: Parser Query
parseExitStmt = do
    _ <- parseLiteral "exit"
    return ExitStmt

parseStatement :: Parser Query
parseStatement =
        parseRentStmt
    <|> parseReturnStmt
    <|> parseCheckStmt
    <|> parseRegisterStmt
 
    <|> parseExitStmt

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = do
    first <- optional p
    case first of
        Nothing -> return []
        Just x -> do xs <- many (sep >> p)
                     return (x:xs)

parseStatements :: Parser [Query]
parseStatements = sepBy parseStatement (parseLiteral ";")

parseProgram :: Parser Query
parseProgram = do
    skipSpaces
    stmts <- parseStatements
    skipSpaces
    return $ if length stmts == 1 then head stmts else Sequence stmts

-- Parses user's input into a Query
parseQuery :: String -> Either String Query
parseQuery input =
    let debugInput = "Debugging input: " ++ input
    in trace debugInput $ 
       let (res, remaining) = runState (runExceptT parseProgram) input in
       case res of
         Left err -> trace ("Error: " ++ err) $ Left err
         Right query ->
           if all isSpace remaining
              then trace ("Parsed successfully: " ++ show query) $ Right query
              else trace ("Unexpected input remaining: " ++ remaining) $ Left $ "Unexpected input: " ++ remaining

