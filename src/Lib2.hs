{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant lambda" #-}

module Lib2
    ( Query(..),
      UserID(..),
      BikeID(..),
      State(..),
      emptyState,
      stateTransition,
      showUserID,
      showBikeID
    ) where

import Data.Char (toLower)
import Debug.Trace (trace)  -- Import trace for debugging

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

showUserID :: UserID -> String
showUserID (UserID n) = "user" ++ show n

showBikeID :: BikeID -> String
showBikeID (BikeID n) = "bike" ++ show n

-- Debug version of stateTransition with trace
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state q =
    trace ("stateTransition called with state=" ++ show state ++ " and query=" ++ show q) $
    case q of
      ExitStmt -> Right (Just "Exiting...", state)
      Sequence queries -> trace "Processing Sequence" $ processQueries state queries
      RentStmt uid bid -> trace ("Handling RentStmt for user=" ++ show uid ++ " and bike=" ++ show bid) $
                          handleRent state uid bid
      ReturnStmt bid uid -> trace ("Handling ReturnStmt for bike=" ++ show bid ++ " and user=" ++ show uid) $
                            handleReturn state bid uid
      CheckBikeStmt bid -> trace ("Handling CheckBikeStmt for bike=" ++ show bid) $
                           handleCheckBike state bid
      CheckUserStmt uid -> trace ("Handling CheckUserStmt for user=" ++ show uid) $
                           handleCheckUser state uid
      RegisterBikeStmt bid -> trace ("Handling RegisterBikeStmt for bike=" ++ show bid) $
                              handleRegisterBike state bid
      RegisterUserStmt uid -> trace ("Handling RegisterUserStmt for user=" ++ show uid) $
                              handleRegisterUser state uid
  where
    processQueries :: State -> [Query] -> Either String (Maybe String, State)
    processQueries st [] = Right (Nothing, st)
    processQueries st (q':qs) =
      trace ("processQueries handling query: " ++ show q') $
      case stateTransition st q' of
        Left err -> trace ("Error: " ++ err) $ Left err
        Right (msg1, st1) -> 
          case processQueries st1 qs of
            Left err -> trace ("Error in subsequent queries: " ++ err) $ Left err
            Right (msg2, st2) -> Right (combineMessages msg1 msg2, st2)

    combineMessages :: Maybe String -> Maybe String -> Maybe String
    combineMessages Nothing m = m
    combineMessages m Nothing = m
    combineMessages (Just m1) (Just m2) = Just (m1 ++ "\n" ++ m2)

    handleRent st uid bid
        | notElem uid (users st) = trace "User not registered." $ Left "User not registered."
        | notElem bid (bikes st) = trace "Bike not registered." $ Left "Bike not registered."
        | any (\(b, _) -> b == bid) (rentedBikes st) = trace "Bike already rented." $ Left "Bike is already rented."
        | otherwise =
            let newState = st { rentedBikes = (bid, uid) : rentedBikes st }
            in trace "Bike rented successfully." $
               Right (Just "Bike rented successfully.", newState)

    handleReturn st bid uid
        | notElem (bid, uid) (rentedBikes st) = trace "User did not rent this bike." $ Left "This user did not rent this bike."
        | otherwise =
            let newRentedBikes = filter (/= (bid, uid)) (rentedBikes st)
                newState = st { rentedBikes = newRentedBikes }
            in trace "Bike returned successfully." $
               Right (Just "Bike returned successfully.", newState)

    handleCheckBike st bid
        | notElem bid (bikes st) = trace "Bike not registered." $ Left "Bike not registered."
        | otherwise =
            case [u | (b, u) <- rentedBikes st, b == bid] of
                [] -> trace "Bike is available." $ Right (Just (showBikeID bid ++ " is available."), st)
                (u:_) -> trace "Bike is rented." $ Right (Just (showBikeID bid ++ " is rented by " ++ showUserID u ++ "."), st)

    handleCheckUser st uid
        | notElem uid (users st) = trace "User not registered." $ Left "User not registered."
        | otherwise =
            let rentedBikesByUser = [bid | (bid, u) <- rentedBikes st, u == uid]
            in if null rentedBikesByUser
                then trace "User has not rented any bikes." $
                     Right (Just (showUserID uid ++ " has not rented any bikes."), st)
                else trace "User has rented bikes." $
                     Right (Just (showUserID uid ++ " has rented: " ++ unwords (map showBikeID rentedBikesByUser)), st)

    handleRegisterBike st bid
        | elem bid (bikes st) = trace "Bike already registered." $ Left "Bike already registered."
        | otherwise =
            let newState = st { bikes = bid : bikes st }
            in trace "Bike registered successfully." $
               Right (Just "Bike registered successfully.", newState)

    handleRegisterUser st uid
        | elem uid (users st) = trace "User already registered." $ Left "User already registered."
        | otherwise =
            let newState = st { users = uid : users st }
            in trace "User registered successfully." $
               Right (Just "User registered successfully.", newState)
