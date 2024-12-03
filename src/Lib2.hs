{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Lib2
    ( Query(..),
      UserID(..),
      BikeID(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      parseStatement,
      Parser(..),
      parseLiteral,
      skipSpaces,
      parseInt,
      parseUserID,
      parseBikeID,
      showUserID,
      showBikeID,
    ) where

import Control.Applicative
import Data.Char (isDigit, isSpace, toLower)

-- | Parser type definition
newtype Parser a = Parser { parse :: String -> Either String (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err -> Left err
        Right (result, rest) -> Right (f result, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    (Parser pf) <*> (Parser p) = Parser $ \input -> case pf input of
        Left err -> Left err
        Right (f, rest) -> case p rest of
            Left err -> Left err
            Right (x, rest') -> Right (f x, rest')

instance Alternative Parser where
    empty = Parser $ \_ -> Left "Parsing failed"
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Left _ -> p2 input
            res -> res

-- ** Added Monad instance **
instance Monad Parser where
    (Parser p) >>= f = Parser $ \input -> case p input of
        Left err -> Left err
        Right (result, rest) -> parse (f result) rest

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

-- Parsing Helpers
skipSpaces :: Parser ()
skipSpaces = Parser $ \input ->
    let rest = dropWhile isSpace input
    in Right ((), rest)

parseLiteral :: String -> Parser String
parseLiteral s = Parser $ \input ->
    let input' = dropWhile isSpace input
        len = length s
        (prefix, rest) = splitAt len input'
    in if map toLower prefix == map toLower s
        then Right (s, rest)
        else Left $ "Expected \"" ++ s ++ "\", but found \"" ++ prefix ++ "\""

parseInt :: Parser Int
parseInt = Parser $ \input ->
    let input' = dropWhile isSpace input
        (digits, rest) = span isDigit input'
    in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)

parseUserID :: Parser UserID
parseUserID = UserID <$> (parseLiteral "user" *> parseInt)

parseBikeID :: Parser BikeID
parseBikeID = BikeID <$> (parseLiteral "bike" *> parseInt)

-- Parsing Functions
parseRentStmt :: Parser Query
parseRentStmt = RentStmt <$> (parseLiteral "rent" *> parseUserID) <*> parseBikeID

parseReturnStmt :: Parser Query
parseReturnStmt = ReturnStmt <$> (parseLiteral "return" *> parseBikeID) <*> parseUserID

parseCheckBikeStmt :: Parser Query
parseCheckBikeStmt = CheckBikeStmt <$> (parseLiteral "check" *> parseLiteral "bike" *> parseBikeID)

parseCheckUserStmt :: Parser Query
parseCheckUserStmt = CheckUserStmt <$> (parseLiteral "check" *> parseLiteral "user" *> parseUserID)

parseCheckStatusStmt :: Parser Query
parseCheckStatusStmt = parseCheckBikeStmt <|> parseCheckUserStmt

parseRegisterBikeStmt :: Parser Query
parseRegisterBikeStmt = RegisterBikeStmt <$> (parseLiteral "register" *> parseLiteral "bike" *> parseBikeID)

parseRegisterUserStmt :: Parser Query
parseRegisterUserStmt = RegisterUserStmt <$> (parseLiteral "register" *> parseLiteral "user" *> parseUserID)

parseExitStmt :: Parser Query
parseExitStmt = ExitStmt <$ parseLiteral "exit"

parseStatement :: Parser Query
parseStatement = parseRentStmt
             <|> parseReturnStmt
             <|> parseCheckStatusStmt
             <|> parseRegisterBikeStmt
             <|> parseRegisterUserStmt
             <|> parseExitStmt

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

parseStatements :: Parser [Query]
parseStatements = sepBy parseStatement (parseLiteral ";")

parseProgram :: Parser Query
parseProgram = do
    stmts <- parseStatements
    return $ if length stmts == 1 then head stmts else Sequence stmts

-- Parses user's input into a Query
parseQuery :: String -> Either String Query
parseQuery input = case parse (skipSpaces *> parseProgram <* skipSpaces) input of
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

-- State Transition Functions
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state ExitStmt = Right (Just "Exiting...", state)
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
            else Right (Just (showUserID uid ++ " has rented: " ++ unwords (map showBikeID rentedBikesByUser)), state)

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
