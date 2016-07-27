{-# LANGUAGE OverloadedStrings #-}

module State.Entry where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Email
import           State.Types.Entry


delete :: Ctxt -> Int -> Int -> IO ()
delete ctxt account_id entry_id = withResource (Context.db ctxt) $ \c -> void $ execute c "DELETE FROM entries WHERE id = ? AND account_id = ?" (entry_id, account_id)

getForAccount :: Ctxt -> Int -> IO [Entry]
getForAccount ctxt account_id =
  withResource (Context.db ctxt) $ \c -> query c "SELECT id, account_id, who, what, date, howmuch, whopays FROM entries WHERE account_id = ? ORDER BY date DESC" (Only account_id)
