{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Test.Tasty.QuickCheck as QC

import Data.Char (isSpace)
import Data.List (sort)
import Lib2 qualified as L2
import Lib3 qualified as L3
import Parsers qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [lib2Tests, lib3Tests, parsersTests]

-- Tests for Lib2
lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 Tests" [lib2UnitTests, lib2PropertyTests]

lib2UnitTests :: TestTree
lib2UnitTests = testGroup "Lib2 Unit Tests"
  [ testCase "Parsing valid RentStmt" $
      Parsers.runParser Parsers.parseStatement "rent user1 bike1" @?= Right (L2.RentStmt (L2.UserID 1) (L2.BikeID 1), ""),
    testCase "State transition with RentStmt" $ do
      let initialState = L2.emptyState { L2.users = [L2.UserID 1], L2.bikes = [L2.BikeID 1] }
      let query = L2.RentStmt (L2.UserID 1) (L2.BikeID 1)
      case L2.stateTransition initialState query of
        Right (Just msg, newState) -> do
          msg @?= "Bike rented successfully."
          L2.rentedBikes newState @?= [(L2.BikeID 1, L2.UserID 1)]
        Left err -> assertFailure err
  ]

lib2PropertyTests :: TestTree
lib2PropertyTests = testGroup "Lib2 Property Tests"
  [ QC.testProperty "Parsing and rendering of UserID" $
      \(Positive n) -> let uid = L2.UserID n
                           str = L2.showUserID uid
                           parseResult = Parsers.runParser Parsers.parseUserID str
                       in case parseResult of
                            Right (uid', rest) -> all isSpace rest && uid' == uid
                            _ -> False,
    QC.testProperty "Parsing and rendering of BikeID" $
      \(Positive n) -> let bid = L2.BikeID n
                           str = L2.showBikeID bid
                           parseResult = Parsers.runParser Parsers.parseBikeID str
                       in case parseResult of
                            Right (bid', rest) -> all isSpace rest && bid' == bid
                            _ -> False
  ]

-- Tests for Lib3
lib3Tests :: TestTree
lib3Tests = testGroup "Lib3 Tests" [lib3UnitTests, lib3PropertyTests]

lib3UnitTests :: TestTree
lib3UnitTests = testGroup "Lib3 Unit Tests"
  [ testCase "Parsing valid Command: save" $
      L3.parseCommand "save" @?= Right (L3.SaveCommand, ""),
    testCase "Parsing valid Command: load" $
      L3.parseCommand "load" @?= Right (L3.LoadCommand, ""),
    testCase "Parsing valid Command: BEGIN ... END" $ do
      let input = "BEGIN\nregister user user1;\nregister bike bike1;\nEND\n"
      case L3.parseCommand input of
        Right (L3.StatementCommand (L3.Batch qs), rest) | all isSpace rest -> length qs @?= 2
        Left err -> assertFailure $ "Failed to parse batch command: " ++ err
        Right (_, rest) -> assertFailure $ "Unexpected input after parsing: " ++ rest
  ]


lib3PropertyTests :: TestTree
lib3PropertyTests = testGroup "Lib3 Property Tests"
  [ QC.testProperty "parseStatements . renderStatements == Right (s, \"\")" $
      \state -> let statements = L3.marshallState (state :: L2.State)
                    rendered = L3.renderStatements statements
                    parseResult = Parsers.runParser L3.parseStatements rendered
                in case parseResult of
                     Right (stmts', rest) -> all isSpace rest && stmts' == statements
                     _ -> False
  ]

-- Tests for Parsers
parsersTests :: TestTree
parsersTests = testGroup "Parsers Tests"
  [ testCase "Parsing UserID" $
      Parsers.runParser Parsers.parseUserID "user1" @?= Right (L2.UserID 1, ""),
    testCase "Parsing BikeID" $
      Parsers.runParser Parsers.parseBikeID "bike1" @?= Right (L2.BikeID 1, ""),
    testCase "Parsing RentStmt" $
      Parsers.runParser Parsers.parseStatement "rent user1 bike1" @?= Right (L2.RentStmt (L2.UserID 1) (L2.BikeID 1), ""),
    testCase "Parsing RegisterUserStmt" $
      Parsers.runParser Parsers.parseStatement "register user user1" @?= Right (L2.RegisterUserStmt (L2.UserID 1), ""),
    testCase "Parsing RegisterBikeStmt" $
      Parsers.runParser Parsers.parseStatement "register bike bike1" @?= Right (L2.RegisterBikeStmt (L2.BikeID 1), "")
  ]

-- Arbitrary instances for QuickCheck
instance Arbitrary L2.UserID where
  arbitrary = L2.UserID . getPositive <$> arbitrary

instance Arbitrary L2.BikeID where
  arbitrary = L2.BikeID . getPositive <$> arbitrary

instance Arbitrary L2.Query where
  arbitrary = oneof
    [ L2.RentStmt <$> arbitrary <*> arbitrary
    , L2.ReturnStmt <$> arbitrary <*> arbitrary
    , L2.CheckBikeStmt <$> arbitrary
    , L2.CheckUserStmt <$> arbitrary
    , L2.RegisterBikeStmt <$> arbitrary
    , L2.RegisterUserStmt <$> arbitrary
    , pure L2.ExitStmt
    ]

instance Arbitrary L2.State where
  arbitrary = do
    users <- listOf arbitrary
    bikes <- listOf arbitrary
    rentedBikes <- listOf ((,) <$> arbitrary <*> arbitrary)
    return L2.State { L2.users = users, L2.bikes = bikes, L2.rentedBikes = rentedBikes }
