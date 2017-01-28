{-# LANGUAGE OverloadedStrings #-}

module State.Person where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Person

create :: Ctxt -> Person -> IO ()
create ctxt person = withResource (Context.db ctxt) $ \c -> void $ execute c "INSERT INTO persons (account_id, name) VALUES (?,?)" (accountId person, name person)

delete :: Ctxt -> Person -> IO ()
delete ctxt person = withResource (Context.db ctxt) $ \c -> void $ execute c "DELETE FROM persons WHERE id = ? AND account_id = ?" (State.Types.Person.id person, accountId person)

getForAccount :: Ctxt -> Int -> IO [Person]
getForAccount ctxt account_id =
  withResource (Context.db ctxt) $ \c -> query c "SELECT id, account_id, name FROM persons WHERE account_id = ? ORDER BY name DESC" (Only account_id)
