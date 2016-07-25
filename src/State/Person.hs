{-# LANGUAGE OverloadedStrings #-}

module State.Person where

import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Person

getForAccount :: Ctxt -> Int -> IO [Person]
getForAccount ctxt account_id =
  withResource (Context.db ctxt) $ \c -> query c "SELECT id, account_id, name FROM persons WHERE account_id = ? ORDER BY name DESC" (Only account_id)
