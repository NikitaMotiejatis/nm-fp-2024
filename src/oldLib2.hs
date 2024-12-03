{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lib2
    ( Query(..),
      UserID(..),
      BikeID(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      Parser,
    ) where

import Data.Char (isDigit, isSpace)
import GHC.Read (parens)

-- | Data types for Queries and State
data Query
  = RentStmt UserID BikeID
  | ReturnStmt BikeID UserID
  | CheckBikeStmt BikeID
  | CheckUserStmt UserID
  | RegisterBikeStmt BikeID
  | RegisterUserStmt UserID
  | ExitStmt
  | Sequence [Query]
  deriving (Eq, Show)

data UserID = UserID Int
  deriving (Eq, Show)

data BikeID = BikeID Int
  deriving (Eq, Show)

data State = State
    { users       :: [UserID],
      bikes       :: [BikeID],
      rentedBikes :: [(BikeID, UserID)]
    } deriving (Eq, Show)

emptyState :: State
emptyState = State { users = [], bikes = [], rentedBikes = [] }

-- | Type alias for the parser.
type Parser a = String -> Either String (a, String)

-- Parsing Helpers
skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- | Parses a specific literal string.
parseLiteral :: String -> Parser String
parseLiteral [] input = Right ([], input)
parseLiteral (x:xs) input =
  let input' = skipSpaces input
  in if null input'
     then Left "Unexpected end of input"
     else if head input' == x
          then case parseLiteral xs (tail input') of
                 Right (str, rest) -> Right (x:str, rest)
                 Left err -> Left err
          else Left $ "Expected \"" ++ (x:xs) ++ "\", but found \"" ++ take (length (x:xs)) input' ++ "\""

-- | Parses an integer number.
parseInt :: Parser Int
parseInt input =
  let input' = skipSpaces input
      (digits, rest) = span isDigit input'
  in if null digits
     then Left "Expected an integer"
     else Right (read digits, rest)

-- | Combines two parsers sequentially.
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' comb p1 p2 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) -> Right (comb v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

-- | Combines three parsers sequentially.
and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' comb p1 p2 p3 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) -> Right (comb v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

-- | Combines multiple parsers with 'orElse'.
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = \input ->
  case p1 input of
    Right r1 -> Right r1
    Left _ -> p2 input

-- Parsing Functions

-- <user_id> ::= "user" [0-9]+
parseUserID :: Parser UserID
parseUserID = and2' (\_ num -> UserID num) (parseLiteral "user") parseInt

-- <bike_id> ::= "bike" [0-9]+
parseBikeID :: Parser BikeID
parseBikeID = and2' (\_ num -> BikeID num) (parseLiteral "bike") parseInt

-- <rent_stmt> ::= "rent" <user_id> <bike_id>
parseRentStmt :: Parser Query
parseRentStmt = and3' (\_ uid bid -> RentStmt uid bid) (parseLiteral "rent") parseUserID parseBikeID

-- <return_stmt> ::= "return" <bike_id> <user_id>
parseReturnStmt :: Parser Query
parseReturnStmt = and3' (\_ bid uid -> ReturnStmt bid uid) (parseLiteral "return") parseBikeID parseUserID

-- Part of <check_status_stmt>: "check" "bike" <bike_id>
parseCheckBikeStmt :: Parser Query
parseCheckBikeStmt = and3' (\_ _ bid -> CheckBikeStmt bid) (parseLiteral "check") (parseLiteral "bike") parseBikeID

-- Part of <check_status_stmt>: "check" "user" <user_id>
parseCheckUserStmt :: Parser Query
parseCheckUserStmt = and3' (\_ _ uid -> CheckUserStmt uid) (parseLiteral "check") (parseLiteral "user") parseUserID

-- <check_status_stmt> ::= "check" "bike" <bike_id> | "check" "user" <user_id>
parseCheckStatusStmt :: Parser Query
parseCheckStatusStmt = parseCheckBikeStmt `orElse` parseCheckUserStmt

-- <register_bike_stmt> ::= "register" "bike" <bike_id>
parseRegisterBikeStmt :: Parser Query
parseRegisterBikeStmt = and3' (\_ _ bid -> RegisterBikeStmt bid) (parseLiteral "register") (parseLiteral "bike") parseBikeID

-- <register_user_stmt> ::= "register" "user" <user_id>
parseRegisterUserStmt :: Parser Query
parseRegisterUserStmt = and3' (\_ _ uid -> RegisterUserStmt uid) (parseLiteral "register") (parseLiteral "user") parseUserID

-- "exit" statement
parseExitStmt :: Parser Query
parseExitStmt input = case parseLiteral "exit" input of
    Right (_, rest) -> Right (ExitStmt, rest)
    Left err -> Left err

-- <statement> ::= <rent_stmt> | <return_stmt> | <check_status_stmt> | <register_bike_stmt> | <register_user_stmt> | "exit"
parseStatement :: Parser Query
parseStatement = foldr1 orElse [parseRentStmt, parseReturnStmt, parseCheckStatusStmt,
                                parseRegisterBikeStmt, parseRegisterUserStmt, parseExitStmt]

-- Helper function to parse multiple statements
parseStatements :: Parser [Query]
parseStatements input = case parseStatement input of
    Right (stmt, rest) ->
        let rest' = skipSpaces rest
        in if not (null rest') && head rest' == ';'
           then case parseStatements (tail rest') of
                  Right (stmts, finalRest) -> Right (stmt:stmts, finalRest)
                  Left err -> Left err
           else Right ([stmt], rest)
    Left err -> Left err

-- <program> ::= <statement> | <statement> ; <program>
parseProgram :: Parser Query
parseProgram input = case parseStatements input of
    Right ([stmt], rest) -> Right (stmt, rest)          
    Right (stmts, rest) -> Right (Sequence stmts, rest) 
    Left err -> Left err

-- Parses user's input into a Query
parseQuery :: String -> Either String Query
parseQuery input = case parseProgram input of
    Right (query, rest) ->
        if all isSpace rest
        then Right query
        else Left $ "Unexpected input: " ++ rest
    Left err -> Left err

-- Helper functions to display UserID and BikeID

showUserID :: UserID -> String
showUserID (UserID n) = "user" ++ show n

showBikeID :: BikeID -> String
showBikeID (BikeID n) = "bike" ++ show n

-- Helper function to display a list of BikeIDs

showBikeIDs :: [BikeID] -> String
showBikeIDs bids = unwords (map showBikeID bids)

-- State Transition Functions

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state ExitStmt = error "Exiting..."
stateTransition state (Sequence queries) = processQueries state queries
  where
    processQueries :: State -> [Query] -> Either String (Maybe String, State)
    processQueries st [] = Right (Nothing, st)
    processQueries st (q:qs) =
      case stateTransition st q of
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

-- Handle RentStmt
stateTransition state (RentStmt uid bid)
    | notElem uid (users state) = Left "User not registered."
    | notElem bid (bikes state) = Left "Bike not registered."
    | elem (bid, uid) (rentedBikes state) = Left "Bike already rented to this user."
    | any (\(b, _) -> b == bid) (rentedBikes state) = Left "Bike is already rented."
    | otherwise =
        let newState = state { rentedBikes = (bid, uid) : rentedBikes state }
        in Right (Just "Bike rented successfully.", newState)

-- Handle ReturnStmt
stateTransition state (ReturnStmt bid uid)
    | notElem (bid, uid) (rentedBikes state) = Left "This user did not rent this bike."
    | otherwise =
        let newRentedBikes = filter (/= (bid, uid)) (rentedBikes state)
            newState = state { rentedBikes = newRentedBikes }
        in Right (Just "Bike returned successfully.", newState)

-- Handle CheckBikeStmt
stateTransition state (CheckBikeStmt bid)
    | notElem bid (bikes state) = Left "Bike not registered."
    | otherwise =
        case [u | (b, u) <- rentedBikes state, b == bid] of
            [] -> Right (Just (showBikeID bid ++ " is available."), state)
            (u:_) -> Right (Just (showBikeID bid ++ " is rented by " ++ showUserID u ++ "."), state)

-- Handle CheckUserStmt
stateTransition state (CheckUserStmt uid)
    | notElem uid (users state) = Left "User not registered."
    | otherwise =
        let rentedBikesByUser = [bid | (bid, u) <- rentedBikes state, u == uid]
        in if null rentedBikesByUser
            then Right (Just (showUserID uid ++ " has not rented any bikes."), state)
            else Right (Just (showUserID uid ++ " has rented: " ++ showBikeIDs rentedBikesByUser), state)

-- Handle RegisterBikeStmt
stateTransition state (RegisterBikeStmt bid)
    | elem bid (bikes state) = Left "Bike already registered."
    | otherwise =
        let newState = state { bikes = bid : bikes state }
        in Right (Just "Bike registered successfully.", newState)

-- Handle RegisterUserStmt
stateTransition state (RegisterUserStmt uid)
    | elem uid (users state) = Left "User already registered."
    | otherwise =
        let newState = state { users = uid : users state }
        in Right (Just "User registered successfully.", newState)
