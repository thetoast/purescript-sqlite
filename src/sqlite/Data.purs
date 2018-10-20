module Sqlite.Data where

import Prelude

import Data.Int.Bits as Bits
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Foreign.Class (class Decode)


--------------------------------------------------------------------------------
-- SqlParam and related classes/instances 
--------------------------------------------------------------------------------
data SqlParam
    = SqlString String
    | SqlInt Int
    | SqlNumber Number
    | SqlBoolean Boolean

type SqlParams = Array (Tuple String SqlParam)

-- Nice printing of SqlParams
instance showSqlParam :: Show SqlParam where
  show (SqlString a)  = a
  show (SqlInt i)     = show i
  show (SqlNumber n)  = show n
  show (SqlBoolean b) = show b

-- Used to convert primitives to their corresponding SqlParam
class SqlParamEncode p where
    mkParam :: p -> SqlParam
instance intParamEncode :: SqlParamEncode Int where
    mkParam = SqlInt
instance numberParamEncode :: SqlParamEncode Number where
    mkParam = SqlNumber
instance stringParamEncode :: SqlParamEncode String where
    mkParam = SqlString
instance booleanParamEncode :: SqlParamEncode Boolean where
    mkParam = SqlBoolean

-- conveneince method for making tuple of string and param from string and primitive
encodeSqlParam :: forall a. SqlParamEncode a => String -> a -> Tuple String SqlParam
encodeSqlParam s p = Tuple s $ mkParam p

-- even more convenient operator
infix 4 encodeSqlParam as :=


--------------------------------------------------------------------------------
-- Database modes
--------------------------------------------------------------------------------
foreign import _OPEN_READONLY :: Int
foreign import _OPEN_READWRITE :: Int
foreign import _OPEN_CREATE :: Int

data DbMode = ReadOnly | ReadWrite | Create | ReadOnlyCreate | ReadWriteCreate

-- convert DbMode to bitwise or of low-level db modes
modeToInt :: DbMode -> Int
modeToInt ReadOnly = _OPEN_READONLY
modeToInt ReadWrite = _OPEN_READWRITE
modeToInt Create = _OPEN_CREATE
modeToInt ReadOnlyCreate = _OPEN_READONLY `Bits.or` _OPEN_CREATE
modeToInt ReadWriteCreate = _OPEN_READWRITE `Bits.or` _OPEN_CREATE


--------------------------------------------------------------------------------
-- Database Event definitions
--------------------------------------------------------------------------------
data DbEvent a
  = Open    (Unit   -> Effect a)
  | Close   (Unit   -> Effect a)
  | Error   (Error  -> Effect a)
  | Trace   (String -> Effect a)
  | Profile (String -> Int -> Effect a)


--------------------------------------------------------------------------------
-- Query and row types
--------------------------------------------------------------------------------
type SqlQuery = String
type SqlRow  a = Decode a => Aff (Maybe a)
type SqlRows a = Decode a => Aff (Array a)


--------------------------------------------------------------------------------
-- Misc foreign types
--------------------------------------------------------------------------------
foreign import data DbStatement :: Type
foreign import data DbConnection :: Type

-- XXX RyanM: previous implementation... not sure if needed
-- | A boxed function that can be used as a listener. This is necessary
-- | due to the underling implementation of Eff functions.
-- foreign import data DbListener :: # Effect -> Type
foreign import data DbListener :: Type


--------------------------------------------------------------------------------
-- Result of a insert, update, or delete
--------------------------------------------------------------------------------
-- | "lastID" result value should be used only for INSERT queries,
-- | "changes" result value should be used only for UPDATE and DELETE queries.
type RunResult = { lastID :: Int, changes :: Int }
