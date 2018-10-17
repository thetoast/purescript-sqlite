module Sqlite.Trans where

import Sqlite.Core
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Foreign.Class (class Decode)
import Data.Maybe (Maybe)
import Prelude (Unit, bind, pure, ($))

type SqlRowT  a = Decode a => ExceptT Error Aff (Maybe a)
type SqlRowsT a = Decode a => ExceptT Error Aff (Array a)


valueToRight
  :: forall a
   . Aff a
  -> Aff (Either Error a)
valueToRight x = do
  y <- x
  pure (Right y)


connectT
  ::String
  -> DbMode
  -> ExceptT Error Aff DbConnection
connectT filename mode = ExceptT (attempt $ connect filename mode)

connectCachedT
  :: String
  -> DbMode
  -> ExceptT Error Aff DbConnection
connectCachedT filename mode = ExceptT (attempt $ connectCached filename mode)

closeT
  :: DbConnection
  -> ExceptT Error Aff Unit
closeT db = ExceptT (attempt $ close db)

runT
  :: DbConnection
  -> SqlQuery
  -> ExceptT Error Aff RunResult
runT db query = ExceptT (attempt $ run db query)

getT
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRowsT a
getT db query = ExceptT $ attempt $ get db query

getOneT
  :: forall a
   . DbConnection
  -> SqlQuery
  -> SqlRowT a
getOneT db query = ExceptT $ attempt $ getOne db query


stmtPrepareT
  ::DbConnection
  -> SqlQuery
  -> ExceptT Error Aff DbStatement
stmtPrepareT db query = ExceptT (attempt $ stmtPrepare db query)

stmtBindT
  :: DbStatement
  -> SqlParams
  -> ExceptT Error Aff Unit
stmtBindT stmt params = ExceptT (attempt $ stmtBind stmt params)

stmtResetT
  :: DbStatement
  -> ExceptT Error Aff Unit
stmtResetT db = ExceptT (valueToRight $ stmtReset db)

stmtFinalizeT
  :: DbStatement
  -> ExceptT Error Aff Unit
stmtFinalizeT db = ExceptT (valueToRight $ stmtFinalize db)

stmtRunT
  :: DbStatement
  -> SqlParams
  -> ExceptT Error Aff RunResult
stmtRunT stmt params = ExceptT (attempt $ stmtRun stmt params)

stmtGetT
  :: forall a
   . DbStatement
  -> SqlParams
  -> SqlRowsT a
stmtGetT stmt query = ExceptT $ attempt $ stmtGet stmt query

-- stmtGetOneT
--   :: forall a
--    . DbStatement
--   -> SqlParams
--   -> SqlRowT a
-- stmtGetOneT stmt query = ExceptT $ attempt $ stmtGetOne stmt query
