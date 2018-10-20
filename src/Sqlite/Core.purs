module Sqlite.Core where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (error)
import Foreign (Foreign)
import Foreign.Class (class Decode, decode)
import Sqlite.Data (DbConnection, DbEvent(..), DbListener, DbMode, DbStatement, RunResult, SqlParams, SqlQuery, SqlRow, SqlRows, modeToInt)


--------------------------------------------------------------------------------
-- Connection related functions
--------------------------------------------------------------------------------
-- | Sets the debug mode for sqlite to verbose
foreign import _setVerbose :: Effect Unit
setVerbose :: Effect Unit
setVerbose = _setVerbose

foreign import _connect :: Fn3 String Int Boolean (EffectFnAff DbConnection)
connect :: String -> DbMode -> Aff DbConnection
connect filename dbMode = fromEffectFnAff $ runFn3 _connect filename mode false
  where
  mode = modeToInt dbMode

-- | Uses sqlite's built-in cache to avoid opening the same database multiple times
connectCached :: String -> DbMode -> Aff DbConnection
connectCached filename dbMode = fromEffectFnAff $ runFn3 _connect filename mode true
  where
  mode = modeToInt dbMode

foreign import _close :: DbConnection -> (EffectFnAff Unit)
close :: DbConnection -> Aff Unit
close = fromEffectFnAff <<<_close

foreign import _listen :: forall a . Fn3 DbConnection String DbListener (Effect a)

-- | Creates a DbListener from a normal PureScript Eff function for the
-- | listen function.
foreign import _dbListener :: forall a b . (a -> Effect b) -> DbListener

-- | Creates a DbListener for a callback that takes two arguments
foreign import _dbListenerFn2 :: forall a b c . (Fn2 a b (Effect c)) -> DbListener

-- | Listener for the database open event
listen :: forall a . DbConnection -> DbEvent a -> Effect a
listen db (Open  callback)   = runFn3 _listen db "open"    (_dbListener callback)
listen db (Close callback)   = runFn3 _listen db "close"   (_dbListener callback)
listen db (Error callback)   = runFn3 _listen db "error"   (_dbListener callback)
listen db (Trace callback)   = runFn3 _listen db "trace"   (_dbListener callback)
listen db (Profile callback) = runFn3 _listen db "profile" (_dbListenerFn2 $ mkFn2 callback)


--------------------------------------------------------------------------------
-- Convenience function for decoding a row
--------------------------------------------------------------------------------
readRow :: forall a. Decode a => Foreign -> Aff a
readRow = decode >>> runExcept >>> either (show >>> error >>> throwError) pure


--------------------------------------------------------------------------------
-- Basic querying functionality
--------------------------------------------------------------------------------
foreign import _run :: Fn2 DbConnection SqlQuery (EffectFnAff RunResult)
run :: DbConnection -> SqlQuery -> Aff RunResult
run db query = fromEffectFnAff $ runFn2 _run db query

foreign import _getOne :: Fn2 DbConnection SqlQuery (EffectFnAff Foreign)
getOne :: forall a . DbConnection -> SqlQuery -> SqlRow a
getOne db query = do
  row <- fromEffectFnAff $ runFn2 _getOne db query
  readRow row

foreign import _get :: Fn2 DbConnection SqlQuery (EffectFnAff (Array Foreign))
get :: forall a . DbConnection -> SqlQuery -> SqlRows a
get db query = do
  rows <- fromEffectFnAff $ runFn2 _get db query
  traverse readRow rows


--------------------------------------------------------------------------------
-- Prepared statements
--------------------------------------------------------------------------------
foreign import _stmtPrepare :: Fn2 DbConnection SqlQuery (EffectFnAff DbStatement)
stmtPrepare :: DbConnection -> SqlQuery -> Aff DbStatement
stmtPrepare db query = fromEffectFnAff $ runFn2 _stmtPrepare db query

foreign import _stmtBind :: Fn2 DbStatement SqlParams (EffectFnAff Unit)
stmtBind :: DbStatement -> SqlParams -> Aff Unit
stmtBind stmt params = fromEffectFnAff $ runFn2 _stmtBind stmt params

foreign import _stmtReset :: DbStatement -> (EffectFnAff Unit)
stmtReset :: DbStatement -> Aff Unit
stmtReset = fromEffectFnAff <<< _stmtReset

foreign import _stmtFinalize :: DbStatement -> (EffectFnAff Unit)
stmtFinalize :: DbStatement -> Aff Unit
stmtFinalize = fromEffectFnAff <<< _stmtFinalize

foreign import _stmtRun :: Fn2 DbStatement SqlParams (EffectFnAff RunResult)
stmtRun :: DbStatement -> SqlParams -> Aff RunResult
stmtRun stmt params = fromEffectFnAff $ runFn2 _stmtRun stmt params

foreign import _stmtGetOne :: Fn2 DbStatement SqlParams (EffectFnAff Foreign)
stmtGetOne :: forall a . DbStatement -> SqlParams -> SqlRow a
stmtGetOne stmt query = do
  row <- fromEffectFnAff $ runFn2 _stmtGetOne stmt query
  readRow row

foreign import _stmtGet :: Fn2 DbStatement SqlParams (EffectFnAff (Array Foreign))
stmtGet :: forall a . DbStatement -> SqlParams -> SqlRows a
stmtGet stmt query = do
  rows <- fromEffectFnAff $ runFn2 _stmtGet stmt query
  traverse readRow rows
