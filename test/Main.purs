module Test.Main where

import Prelude (Unit)
import Effect (Effect)
import Test.Sqlite.Core (main) as Sqlite
import Test.Sqlite.Trans (main) as SqliteTrans


main :: Effect Unit
main = do
  Sqlite.main
  -- SqliteTrans.main
