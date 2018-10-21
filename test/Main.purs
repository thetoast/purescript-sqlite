module Test.Main where

import Prelude

import Effect (Effect)
import Test.Sqlite.Core (main) as Sqlite
import Test.Sqlite.Trans (main) as SqliteTrans
import Test.Sqlite.Data (main) as SqliteData


main :: Effect Unit
main = do
  Sqlite.main
  SqliteTrans.main
  SqliteData.main
