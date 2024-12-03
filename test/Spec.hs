{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck (Arbitrary(..), Positive(..))

import Data.List
import Data.Maybe (isJust)
import Data.Char (isSpace)
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent (newChan)
import Control.Monad (void)

import qualified Lib2
import qualified Lib3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [lib2Tests, lib3Tests]

-- Tests for Lib2
lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 Tests" [lib2UnitTests, lib2PropertyTests]

lib2UnitTests :: TestTree
lib2UnitTests = testGroup "Lib2 Unit Tests"
  [ testCase "Parsing valid RentStmt" $
      Lib2.parseQuery "rent user1 bike1" @?= Right (Lib2.RentStmt (Lib2.UserID 1) (Lib2.BikeID 1)),
    testCase "Parsing invalid command" $
      case Lib2.parseQuery "invalid command" of
        Left _ -> return ()
        Right _ -> assertFailure "Expected parse error",
    testCase "State transition with RentStmt" $ do
      let initialState = Lib2.emptyState { Lib2.users = [Lib2.UserID 1], Lib2.bikes = [Lib2.BikeID 1] }
      let query = Lib2.RentStmt (Lib2.UserID 1) (Lib2.BikeID 1)
      case Lib2.stateTransition initialState query of
        Right (Just msg, newState) -> do
          msg @?= "Bike rented successfully."
          Lib2.rentedBikes newState @?= [(Lib2.BikeID 1, Lib2.UserID 1)]
        Left err -> assertFailure err
  ]

lib2PropertyTests :: TestTree
lib2PropertyTests = testGroup "Lib2 Property Tests"
  [
    QC.testProperty "Parsing and rendering of UserID" $
      \(Positive n) -> let uid = Lib2.UserID n
                           str = Lib2.showUserID uid
                           parseResult = Lib2.parse Lib2.parseUserID str
                       in case parseResult of
                            Right (uid', rest) -> all isSpace rest && uid' == uid
                            _ -> False,
    QC.testProperty "Parsing and rendering of BikeID" $
      \(Positive n) -> let bid = Lib2.BikeID n
                           str = Lib2.showBikeID bid
                           parseResult = Lib2.parse Lib2.parseBikeID str
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
      Lib3.parseCommand "save" @?= Right (Lib3.SaveCommand, ""),
    testCase "Parsing valid Command: load" $
      Lib3.parseCommand "load" @?= Right (Lib3.LoadCommand, ""),
    testCase "Parsing valid Command: BEGIN ... END" $ do
      let input = "BEGIN\nregister user user1;\nregister bike bike1;\nEND\n"
      case Lib3.parseCommand input of
        Right (Lib3.StatementCommand (Lib3.Batch qs), rest) | all isSpace rest -> length qs @?= 2
        Left err -> assertFailure $ "Failed to parse batch command: " ++ err
        Right (_, rest) -> assertFailure $ "Unexpected input after parsing: " ++ rest,
    testCase "State marshalling and unmarshalling" $ do
      let state = Lib2.State { Lib2.users = [Lib2.UserID 1], Lib2.bikes = [Lib2.BikeID 1], Lib2.rentedBikes = [(Lib2.BikeID 1, Lib2.UserID 1)] }
      let statements = Lib3.marshallState state
      let rendered = Lib3.renderStatements statements
      case Lib2.parse Lib3.parseStatements rendered of
        Right (stmts', rest) | all isSpace rest -> stmts' @?= statements
        Left err -> assertFailure err
        Right (_, rest) -> assertFailure $ "Unexpected input after parsing: " ++ rest
  ]

lib3PropertyTests :: TestTree
lib3PropertyTests = testGroup "Lib3 Property Tests"
  [
    QC.testProperty "parseStatements . renderStatements == Right (s, \"\")" $
      \state -> let statements = Lib3.marshallState (state :: Lib2.State)
                    rendered = Lib3.renderStatements statements
                    parseResult = Lib2.parse Lib3.parseStatements rendered
                in case parseResult of
                     Right (stmts', rest) -> all isSpace rest && stmts' == statements
                     _ -> False
  ]

-- Arbitrary instances for QuickCheck

instance Arbitrary Lib2.UserID where
  arbitrary = Lib2.UserID . getPositive <$> arbitrary

instance Arbitrary Lib2.BikeID where
  arbitrary = Lib2.BikeID . getPositive <$> arbitrary

instance Arbitrary Lib2.Query where
  arbitrary = oneof
    [ Lib2.RentStmt <$> arbitrary <*> arbitrary
    , Lib2.ReturnStmt <$> arbitrary <*> arbitrary
    , Lib2.CheckBikeStmt <$> arbitrary
    , Lib2.CheckUserStmt <$> arbitrary
    , Lib2.RegisterBikeStmt <$> arbitrary
    , Lib2.RegisterUserStmt <$> arbitrary
    , pure Lib2.ExitStmt
    ]

instance Arbitrary Lib2.State where
  arbitrary = do
    users <- listOf arbitrary
    bikes <- listOf arbitrary
    let rentedBikesGen = if null users || null bikes
                         then return []
                         else listOf ((,) <$> elements bikes <*> elements users)
    rentedBikes <- rentedBikesGen
    return Lib2.State { Lib2.users = users, Lib2.bikes = bikes, Lib2.rentedBikes = rentedBikes }
