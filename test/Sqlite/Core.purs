module Test.Sqlite.Core where

import Prelude

import Data.Array (length)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect, forE)
import Effect.Aff (attempt, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Foreign.Class (class Decode, decode)
import Foreign.Index (readProp)
import Sqlite.Core (close, connect, get, listen, run, stmtFinalize, stmtGet, stmtGetOne, stmtPrepare, stmtRun)
import Sqlite.Data (DbConnection, DbEvent(..), DbMode(..), SqlRows, (:=))
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

instance decodeLorem :: Decode Lorem where
  decode obj = do
    n <- decode =<< readProp "info" obj
    pure $ Lorem { info: n }

instance loremEq :: Eq Lorem where
  eq (Lorem a) (Lorem b) = a.info == b.info

instance showLorem :: Show Lorem where
    show (Lorem a) = show a.info

data Lorem = Lorem { info :: String }


testConnectionListeners
  :: DbConnection
  -> Effect Unit
testConnectionListeners db = do
  _ <- listen db (Open  (\_ -> launchAff $ assert "Db listener 'open' called" true))
  _ <- listen db (Close (\_ -> launchAff $ assert "Db listener 'close' called" true))
  _ <- listen db (Error (\err -> launchAff $ assert (message err) false))
  --listen db (Trace (\str -> log str))
  --listen db (Profile (\str time -> log $ "(" <> show time <> "ms) " <> str))
  pure unit


main :: Effect Unit
main = runTest do
  suite "Sqlite.Core" do
    test "functionality" do
      db <- connect ":memory:" ReadWriteCreate
      _ <- liftEffect $ testConnectionListeners db
      _ <- run db "CREATE TABLE IF NOT EXISTS lorem (info TEXT)"
      stmt <- stmtPrepare db "INSERT INTO lorem VALUES ($value)"

      _ <- liftEffect $ forE 0 10 \i -> do
        _ <- launchAff $ stmtRun stmt [ "$value" := i ]
        pure unit

      stmtFinalize stmt

      rows <- get db "SELECT * from lorem"
      assert "Rows do not match expected output" $ rows == map (\x -> Lorem {info: show x}) [0,1,2,3,4,5,6,7,8,9]

      stmtSelectLimit <- stmtPrepare db "SELECT * FROM lorem LIMIT $limit"
      let rowsLimit = 5
      limitedRows <- stmtGet stmtSelectLimit [ "$limit" := rowsLimit ] :: SqlRows Lorem
      assert "Rows number does not match limit" $ length limitedRows == rowsLimit
      stmtFinalize stmtSelectLimit

      stmtSelectOne <- stmtPrepare db "SELECT * FROM lorem WHERE info = $info"
      let loremInfo = "5"
      loremRow <- stmtGetOne stmtSelectOne [ "$info" := loremInfo ]
      assert "Expected row is not found" $ loremRow == (Just $ Lorem { info: loremInfo })
      let badLoremInfo = "11"
      badLoremRow <- stmtGetOne stmtSelectOne [ "$info" := badLoremInfo ]
      assert "Expected row to be Nothing" $ isNothing (badLoremRow :: Maybe Lorem)
      stmtFinalize stmtSelectOne

      close db

    test "failed connect" do
      failDb <- attempt $ connect "someNonexistentFile" ReadOnly
      assert "Db connection failed" $ (isLeft failDb) == true

      case failDb of
        Right _  -> assert "Db connection should not have succeeded" false
        Left err -> assert "Db error object has the wrong message" $ (message err) == "SQLITE_CANTOPEN: unable to open database file"

    test "setVerbose" do
      -- setVerbose call can't fail
      -- This test should somehow check stacktrace
      -- of a failed sqlite method call
      pure unit
