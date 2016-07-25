{-# LANGUAGE OverloadedStrings #-}

module State.Entry where

import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Email
import           State.Types.Entry

getForAccount :: Ctxt -> Int -> IO [Entry]
getForAccount ctxt account_id =
  withResource (Context.db ctxt) $ \c -> query c "SELECT id, account_id, who, what, date, howmuch, whopays FROM entries WHERE account_id = ? ORDER BY date DESC" (Only account_id)
