module Sqlite.Core where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (Error, error)
import Foreign (Foreign)
import Foreign.Class (class Decode, decode)


foreign import _OPEN_READONLY :: Int
foreign import _OPEN_READWRITE :: Int
foreign import _OPEN_CREATE :: Int

-- | The file mode when connection to the database
data DbMode = ReadOnly | ReadWrite | Create | ReadOnlyCreate | ReadWriteCreate

modeToInt :: DbMode -> Int
modeToInt ReadOnly = _OPEN_READONLY
modeToInt ReadWrite = _OPEN_READWRITE
modeToInt Create = _OPEN_CREATE
modeToInt ReadOnlyCreate = _OPEN_READONLY `Bits.or` _OPEN_CREATE
modeToInt ReadWriteCreate = _OPEN_READWRITE `Bits.or` _OPEN_CREATE

-- | Corresponds to a database event
data DbEvent a
  = Open    (Unit   -> Effect a)
  | Close   (Unit   -> Effect a)
  | Error   (Error  -> Effect a)
  | Trace   (String -> Effect a)
  | Profile (String -> Int -> Effect a)

data SqlParam
    = SqlString String
    | SqlInt Int
    | SqlNumber Number
    | SqlBoolean Boolean

class SqlParamEncode p where
    mkParam2 :: String -> p -> Tuple String SqlParam
instance intParamEncode :: SqlParamEncode Int where
    mkParam2 k i = Tuple k (SqlInt i)
instance numberParamEncode :: SqlParamEncode Number where
    mkParam2 k f = Tuple k (SqlNumber f)
instance stringParamEncode :: SqlParamEncode String where
    mkParam2 k s = Tuple k (SqlString s)
instance booleanParamEncode :: SqlParamEncode Boolean where
    mkParam2 k b = Tuple k (SqlBoolean b)

infix 4 mkParam2 as :=

instance showSqlParam :: Show SqlParam where
  show (SqlString a)  = a
  show (SqlInt i)     = show i
  show (SqlNumber n)  = show n
  show (SqlBoolean b) = show b

type SqlParams = Array (Tuple String SqlParam)

type SqlQuery = String
type SqlRow  a = Decode a => Aff (Maybe a)
type SqlRows a = Decode a => Aff (Array a)


-- | Sets the debug mode for sqlite to verbose
setVerbose :: Effect Unit
setVerbose = _setVerbose


connect
  :: String
  -> DbMode
  -> Aff DbConnection
connect filename dbMode = fromEffectFnAff $ runFn3 _connect filename mode true
  where
  mode = modeToInt dbMode

-- | Uses sqlite's built-in cache to avoid opening the same database multiple times
connectCached
  :: String
  -> DbMode
  -> Aff DbConnection
connectCached filename dbMode = fromEffectFnAff $ runFn3 _connect filename mode true
  where
  mode = modeToInt dbMode


close
  :: DbConnection
  -> Aff Unit
close = fromEffectFnAff <<<_close


-- | "lastID" result value should be used only for INSERT queries,
-- | "changes" result value should be used only for UPDATE and DELETE queries.
type RunResult = { lastID :: Int, changes :: Int }


run
  :: DbConnection
  -> SqlQuery
  -> Aff RunResult
run db query = fromEffectFnAff $ runFn2 _run db query


readRow :: forall a. Decode a => Foreign -> Aff a
readRow = decode >>> runExcept >>> either (show >>> error >>> throwError) pure


getOne
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRow a
getOne db query = do
  row <- fromEffectFnAff $ runFn2 _getOne db query
  traverse readRow row


get
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRows a
get db query = do
  rows <- fromEffectFnAff $ runFn2 _get db query
  traverse readRow rows


stmtPrepare
  :: DbConnection
  -> SqlQuery
  -> Aff DbStatement
stmtPrepare db query = fromEffectFnAff $ runFn2 _stmtPrepare db query


stmtBind
  :: DbStatement
  -> SqlParams
  -> Aff Unit
stmtBind stmt params = fromEffectFnAff $ runFn2 _stmtBind stmt params


stmtReset
  :: DbStatement
  -> Aff Unit
stmtReset = fromEffectFnAff <<< _stmtReset


stmtFinalize
  :: DbStatement
  -> Aff Unit
stmtFinalize = fromEffectFnAff <<< _stmtFinalize


stmtRun
  :: DbStatement
  -> SqlParams
  -> Aff RunResult
stmtRun stmt params = fromEffectFnAff $ runFn2 _stmtRun stmt params


stmtGetOne
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRow a
stmtGetOne stmt query = do
  row <- fromEffectFnAff $ runFn2 _stmtGetOne stmt query
  readRow row

stmtGet
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRows a
stmtGet stmt query = do
  rows <- fromEffectFnAff $ runFn2 _stmtGet stmt query
  traverse readRow rows


-- | Listener for the database open event
listen
  :: forall a
   . DbConnection
  -> DbEvent a
  -> Effect a
listen db (Open  callback)   = runFn3 _listen db "open"    (_dbListener callback)
listen db (Close callback)   = runFn3 _listen db "close"   (_dbListener callback)
listen db (Error callback)   = runFn3 _listen db "error"   (_dbListener callback)
listen db (Trace callback)   = runFn3 _listen db "trace"   (_dbListener callback)
listen db (Profile callback) = runFn3 _listen db "profile" (_dbListenerFn2 $ mkFn2 callback)


foreign import data DbStatement :: Type

foreign import data DbConnection :: Type

-- | A boxed function that can be used as a listener. This is necessary
-- | due to the underling implementation of Eff functions.
-- foreign import data DbListener :: # Effect -> Type
foreign import data DbListener :: Type

-- | Creates a DbListener from a normal PureScript Eff function for the
-- | listen function.
foreign import _dbListener
  :: forall a b
   . (a -> Effect b)
  -> DbListener

-- | Creates a DbListener for a callback that takes two arguments
foreign import _dbListenerFn2
  :: forall a b c
   . (Fn2 a b (Effect c))
  -> DbListener

foreign import _listen
  :: forall a
   . Fn3
     DbConnection
     String
     DbListener
     (Effect a)

foreign import _setVerbose :: Effect Unit

foreign import _connect
  :: Fn3
     String
     Int
     Boolean
     (EffectFnAff DbConnection)

foreign import _close
  :: DbConnection
  -> (EffectFnAff Unit)

foreign import _run
  :: Fn2
     DbConnection
     SqlQuery
     (EffectFnAff RunResult)

foreign import _getOne
  :: Fn2
     DbConnection
     SqlQuery
     (EffectFnAff (Maybe Foreign))

foreign import _get
  :: Fn2
     DbConnection
     SqlQuery
     (EffectFnAff (Array Foreign))


foreign import _stmtPrepare
  :: Fn2
     DbConnection
     SqlQuery
     (EffectFnAff DbStatement)

foreign import _stmtBind
  :: Fn2
     DbStatement
     SqlParams
     (EffectFnAff Unit)

foreign import _stmtReset
  :: DbStatement
  -> (EffectFnAff Unit)

foreign import _stmtFinalize
  :: DbStatement
  -> (EffectFnAff Unit)

foreign import _stmtRun
  :: Fn2
     DbStatement
     SqlParams
     (EffectFnAff RunResult)

foreign import _stmtGetOne
  :: Fn2
     DbStatement
     SqlParams
     (EffectFnAff Foreign)

foreign import _stmtGet
  :: Fn2
     DbStatement
     SqlParams
     (EffectFnAff (Array Foreign))
