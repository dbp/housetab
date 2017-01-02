{-# LANGUAGE OverloadedStrings #-}

module State.Entry where

import           Control.Monad                    (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types (PGArray (..))

import           Context
import           State.Types.Email
import           State.Types.Entry


create :: Ctxt -> Int -> Entry -> IO ()
create ctxt account_id entry = withResource (Context.db ctxt) $ \c -> void $ execute c "INSERT INTO entries (account_id, who, what, date, howmuch, whopays, category) VALUES (?,?,?,?,?,?, '')" (account_id, whoId entry, description entry, date entry, howmuch entry, PGArray $ whopaysIds entry)

delete :: Ctxt -> Int -> Int -> IO ()
delete ctxt account_id entry_id = withResource (Context.db ctxt) $ \c -> void $ execute c "DELETE FROM entries WHERE id = ? AND account_id = ?" (entry_id, account_id)

get :: Ctxt -> Int -> Int -> IO (Maybe Entry)
get ctxt account_id id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, account_id, who, what, date, howmuch, whopays FROM entries WHERE id = ? AND account_id = ?" (id', account_id)

update :: Ctxt -> Int -> Entry -> IO ()
update ctxt account_id entry = withResource (Context.db ctxt) $ \c -> void $ execute c "UPDATE entries SET who = ?, what = ?, date = ?, howmuch = ?, whopays = ? WHERE id = ? AND account_id = ?" (whoId entry, description entry, date entry, howmuch entry, PGArray $ whopaysIds entry, State.Types.Entry.id entry, account_id)

getForAccount :: Ctxt -> Int -> IO [Entry]
getForAccount ctxt account_id =
  withResource (Context.db ctxt) $ \c -> query c "SELECT id, account_id, who, what, date, howmuch, whopays FROM entries WHERE account_id = ? AND archived IS NULL ORDER BY date DESC" (Only account_id)
