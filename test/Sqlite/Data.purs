module Test.Sqlite.Data where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Sqlite.Data (DbMode(..), modeToInt, SqlParam(..), (:=))
import Test.Unit (test, suite)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Sqlite.Data" do

    test "modeToInt" do
      let roMode = modeToInt ReadOnly
      assert "read only should be '1'" $ roMode == 1
      let rwMode = modeToInt ReadWrite
      assert "read write should be '2'" $ rwMode == 2
      let cMode = modeToInt Create
      assert "create should be '4'" $ cMode == 4
      let rocMode = modeToInt ReadOnlyCreate
      assert "read create should be '5'" $ rocMode == 5
      let rwcMode = modeToInt ReadWriteCreate
      assert "write create should be '6'" $ rwcMode == 6
      
      pure unit

    test "showSqlParam" do
      let foo = show (SqlString "foo")
      assert "string show not working" $ foo == "foo" 
      let one = show (SqlInt 1)
      assert "int show not working" $ one == "1"
      let two = show (SqlNumber 2.0)
      assert "number show not working" $ two == "2.0"
      let tru = show (SqlBoolean true)
      assert "boolean show not working" $ tru == "true"

      pure unit

    test "eqSqlParam" do
      let s1 = SqlString "foo"
      let s2 = SqlString "bar"
      let s3 = SqlString "foo"
      assert "SqlStrings should not be eq" $ s1 /= s2
      assert "SqlStrings should be eq" $ s1 == s3

      let i1 = SqlInt 1
      let i2 = SqlInt 2
      let i3 = SqlInt 1
      assert "SqlInts should not be eq" $ i1 /= i2
      assert "SqlInts should be eq" $ i1 == i3

      let n1 = SqlNumber 1.0
      let n2 = SqlNumber 2.0
      let n3 = SqlNumber 1.0
      assert "SqlNumbers should not be eq" $ n1 /= n2
      assert "SqlNumbers should be eq" $ n1 == n3

      let b1 = SqlBoolean true
      let b2 = SqlBoolean false
      let b3 = SqlBoolean true
      assert "SqlBooleans should not be eq" $ b1 /= b2
      assert "SqlBooleans should be eq" $ b1 == b3

      assert "Different types must not be eq" $ s1 /= i1

      pure unit

    test "encodeSqlParam" do
      assert "param operator did not convert string" $
        ("key" := "value") == (Tuple "key" (SqlString "value"))

      assert "param operator did not convert int" $
        ("key" := 1) == (Tuple "key" (SqlInt 1))

      assert "param operator did not convert number" $
        ("key" := 1.0) == (Tuple "key" (SqlNumber 1.0))

      assert "param operator did not convert boolean" $
        ("key" := true) == (Tuple "key" (SqlBoolean true))

      pure unit

-- --------------------------------------------------------------------------------
-- -- Database Event definitions
-- --------------------------------------------------------------------------------
-- data DbEvent a
--   = Open    (Unit   -> Effect a)
--   | Close   (Unit   -> Effect a)
--   | Error   (Error  -> Effect a)
--   | Trace   (String -> Effect a)
--   | Profile (String -> Int -> Effect a)
-- 
-- 
-- --------------------------------------------------------------------------------
-- -- Query and row types
-- --------------------------------------------------------------------------------
-- type SqlQuery = String
-- type SqlRow  a = Decode a => Aff (Maybe a)
-- type SqlRows a = Decode a => Aff (Array a)
-- 
-- 
-- --------------------------------------------------------------------------------
-- -- Misc foreign types
-- --------------------------------------------------------------------------------
-- foreign import data DbStatement :: Type
-- foreign import data DbConnection :: Type
-- 
-- -- XXX RyanM: previous implementation... not sure if needed
-- -- | A boxed function that can be used as a listener. This is necessary
-- -- | due to the underling implementation of Eff functions.
-- -- foreign import data DbListener :: # Effect -> Type
-- foreign import data DbListener :: Type
-- 
-- 
-- --------------------------------------------------------------------------------
-- -- Result of a insert, update, or delete
-- --------------------------------------------------------------------------------
-- -- | "lastID" result value should be used only for INSERT queries,
-- -- | "changes" result value should be used only for UPDATE and DELETE queries.
-- type RunResult = { lastID :: Int, changes :: Int }
