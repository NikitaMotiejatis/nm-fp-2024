{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
    testCase "Parsing rent command" $
      parseQuery "rent user1 bike1" @?=
        Right (RentStmt (UserID 1) (BikeID 1)),

    testCase "Parsing multiple commands" $
      parseQuery "register user user1; register bike bike1; rent user1 bike1" @?=
        Right (Sequence [ RegisterUserStmt (UserID 1),
                          RegisterBikeStmt (BikeID 1),
                          RentStmt (UserID 1) (BikeID 1) ]),

    testCase "Parsing with extra whitespace" $
      parseQuery "  register   bike   bike5  " @?=
        Right (RegisterBikeStmt (BikeID 5)),

    testCase "Parsing invalid command" $
      case parseQuery "invalid command" of
        Left _  -> () @?= ()
        Right _ -> error "Expected parsing to fail",

    testCase "State transition - check user after renting a bike" $
      let initialState = emptyState
          Right (_, state1) = stateTransition initialState (RegisterUserStmt (UserID 1))
          Right (_, state2) = stateTransition state1 (RegisterBikeStmt (BikeID 1))
          Right (_, state3) = stateTransition state2 (RentStmt (UserID 1) (BikeID 1))
          Right (Just msg, _) = stateTransition state3 (CheckUserStmt (UserID 1))
      in msg @?= "user1 has rented: bike1",

    testCase "State transition - register user" $
      let initialState = emptyState
          Right (Just msg, state1) = stateTransition initialState (RegisterUserStmt (UserID 1))
      in do
          msg @?= "User registered successfully."
          state1 @?= initialState { users = [UserID 1] },

    testCase "State transition - rent bike without registration" $
      case stateTransition emptyState (RentStmt (UserID 1) (BikeID 1)) of
        Left err -> err @?= "User not registered."
        Right _  -> error "Expected operation to fail"
  ]
