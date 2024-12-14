{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Lens hiding (view)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.String.Conversions (cs)
import Network.Wreq hiding (get)
import System.Environment (getArgs)

-- Command Data Type
data Command next
  = RegisterUser String next     -- User ID
  | RegisterBike String next     -- Bike ID
  | RentBike String String next  -- User ID, Bike ID
  | ReturnBike String String next-- Bike ID, User ID
  | CheckBike String (String -> next) -- Bike ID
  | CheckUser String (String -> next) -- User ID
  | View (String -> next)       -- View the current state
  | Save next                   -- Save the state
  | Load (String -> next)       -- Load the state
  deriving (Functor)

type BikeDSL = Free Command

-- Smart Constructors
save :: BikeDSL ()
save = liftF $ Save ()

load :: BikeDSL String
load = liftF $ Load id

registerUser :: String -> BikeDSL ()
registerUser userId = liftF $ RegisterUser userId ()

registerBike :: String -> BikeDSL ()
registerBike bikeId = liftF $ RegisterBike bikeId ()

rentBike :: String -> String -> BikeDSL ()
rentBike userId bikeId = liftF $ RentBike userId bikeId ()

returnBike :: String -> String -> BikeDSL ()
returnBike bikeId userId = liftF $ ReturnBike bikeId userId ()

checkBike :: String -> BikeDSL String
checkBike bikeId = liftF $ CheckBike bikeId id

checkUser :: String -> BikeDSL String
checkUser userId = liftF $ CheckUser userId id

view :: BikeDSL String
view = liftF $ View id

-- HTTP Request per Command (Single)
runHttpSingle :: BikeDSL a -> IO a
runHttpSingle (Pure a) = return a
runHttpSingle (Free (RegisterUser userId next)) = do
  putStrLn $ "Sending request: register user(" ++ userId ++ ")"
  _ <- post "http://localhost:3000" (cs $ "register user " ++ userId :: ByteString)
  runHttpSingle next

runHttpSingle (Free (RegisterBike bikeId next)) = do
  putStrLn $ "Sending request: register bike(" ++ bikeId ++ ")"
  _ <- post "http://localhost:3000" (cs $ "register bike " ++ bikeId :: ByteString)
  runHttpSingle next

runHttpSingle (Free (RentBike userId bikeId next)) = do
  putStrLn $ "Sending request: rent(" ++ userId ++ ", " ++ bikeId ++ ")"
  _ <- post "http://localhost:3000" (cs $ "rent " ++ userId ++ " " ++ bikeId :: ByteString)
  runHttpSingle next

runHttpSingle (Free (ReturnBike bikeId userId next)) = do
  putStrLn $ "Sending request: return(" ++ bikeId ++ ", " ++ userId ++ ")"
  _ <- post "http://localhost:3000" (cs $ "return " ++ bikeId ++ " " ++ userId :: ByteString)
  runHttpSingle next

runHttpSingle (Free (CheckBike bikeId next)) = do
  putStrLn $ "Sending request: check bike(" ++ bikeId ++ ")"
  resp <- post "http://localhost:3000" (cs $ "check bike " ++ bikeId :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpSingle (Free (CheckUser userId next)) = do
  putStrLn $ "Sending request: check user(" ++ userId ++ ")"
  resp <- post "http://localhost:3000" (cs $ "check user " ++ userId :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpSingle (Free (View next)) = do
  putStrLn "Sending request: view"
  resp <- post "http://localhost:3000" (cs "view" :: ByteString)
  putStrLn $ "Received response: " ++ cs (resp ^. responseBody)
  runHttpSingle (next $ cs $ resp ^. responseBody)

runHttpSingle (Free (Save next)) = do
  putStrLn "Sending request: save"
  _ <- post "http://localhost:3000" (cs "save" :: ByteString)
  runHttpSingle next

runHttpSingle (Free (Load next)) = do
  putStrLn "Sending request: load"
  resp <- post "http://localhost:3000" (cs "load" :: ByteString)
  runHttpSingle (next $ cs $ resp ^. responseBody)

-- HTTP Request per Command (Batch)
runHttpBatch :: BikeDSL a -> IO a
runHttpBatch = runHttpBatch' []

runHttpBatch' :: [String] -> BikeDSL a -> IO a
runHttpBatch' acc (Pure a) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  return a

runHttpBatch' acc (Free (RegisterUser userId next)) =
  runHttpBatch' (acc ++ ["register user " ++ userId]) next

runHttpBatch' acc (Free (RegisterBike bikeId next)) =
  runHttpBatch' (acc ++ ["register bike " ++ bikeId]) next

runHttpBatch' acc (Free (RentBike userId bikeId next)) =
  runHttpBatch' (acc ++ ["rent " ++ userId ++ " " ++ bikeId]) next

runHttpBatch' acc (Free (ReturnBike bikeId userId next)) =
  runHttpBatch' (acc ++ ["return " ++ bikeId ++ " " ++ userId]) next

runHttpBatch' acc (Free (CheckBike bikeId next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs $ "check bike " ++ bikeId :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

runHttpBatch' acc (Free (CheckUser userId next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs $ "check user " ++ userId :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

runHttpBatch' acc (Free (View next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  resp <- post "http://localhost:3000" (cs "view" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

runHttpBatch' acc (Free (Save next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  putStrLn "Sending request: save"
  _ <- post "http://localhost:3000" (cs "save" :: ByteString)
  runHttpBatch' [] next

runHttpBatch' acc (Free (Load next)) = do
  unless (null acc) $ do
    putStrLn $ "Sending batch request: " ++ unlines acc
    _ <- post "http://localhost:3000" (cs $ unlines acc :: ByteString)
    return ()
  putStrLn "Sending request: load"
  resp <- post "http://localhost:3000" (cs "load" :: ByteString)
  runHttpBatch' [] (next $ cs $ resp ^. responseBody)

-- In-Memory Interpreter
type InMemoryState = [(String, String)]

runInMemory :: BikeDSL a -> State InMemoryState a
runInMemory (Pure a) = return a
runInMemory (Free (RegisterUser userId next)) = do
  modify ((userId, "User Registered") :)
  runInMemory next
runInMemory (Free (RegisterBike bikeId next)) = do
  modify ((bikeId, "Bike Registered") :)
  runInMemory next
runInMemory (Free (RentBike userId bikeId next)) = do
  modify ((userId ++ " rents " ++ bikeId, "Rented") :)
  runInMemory next
runInMemory (Free (ReturnBike bikeId userId next)) = do
  modify (filter (\(entry, _) -> not (entry == userId ++ " rents " ++ bikeId)))
  runInMemory next
runInMemory (Free (CheckBike bikeId next)) = do
  currentState <- get
  runInMemory (next $ maybe "Available" id (lookup bikeId currentState))
runInMemory (Free (CheckUser userId next)) = do
  currentState <- get
  let rentedBikes = [bike | (user, bike) <- currentState, user == userId]
  runInMemory (next $ unwords rentedBikes)
runInMemory (Free (View next)) = do
  currentState <- get
  runInMemory (next $ show currentState)
runInMemory (Free (Save next)) = do
  -- Saving is a no-op in memory (just keep the current state)
  runInMemory next
runInMemory (Free (Load next)) = do
  -- Loading returns the current state as a string
  currentState <- get
  runInMemory (next $ show currentState)

-- Main Program
main :: IO ()
main = do
  args <- getArgs
  let program = do
        load
        registerUser "user3"
        registerBike "bike3"
        rentBike "user3" "bike3"
        view

  case args of
    ["single"] -> do
      putStrLn "Running with HTTP single request per command:"
      _ <- runHttpSingle program
      return ()
    ["batch"] -> do
      putStrLn "Running with HTTP batch requests:"
      _ <- runHttpBatch program
      return ()
    ["memory"] -> do
      putStrLn "Running with in-memory interpreter for testing:"
      let (result, finalState) = runState (runInMemory program) []
      print result
      print finalState
    _ -> putStrLn "Usage: stack run fp2024-four-client [single|batch|memory]"
