{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module State.Set where

import           Control.Monad                    (void)
import           Data.Maybe                       (listToMaybe)
import           Data.Pool
import           Data.Text                        (Text)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types (PGArray (..))

import           Context
import           State.Types.Entry                (Entry)
import qualified State.Types.Entry                as Entry
import           State.Types.Set


getEntries :: Ctxt -> Int -> Int -> IO [Entry]
getEntries ctxt account_id id' = withResource (Context.db ctxt) $ \c -> query c "SELECT id, account_id, who, what, date, howmuch, whopays FROM entries JOIN entry_sets ON entry_id = id WHERE account_id = ? AND set_id = ?" (account_id, id')

createExport :: Ctxt -> Int -> IO ()
createExport ctxt account_id = withResource (Context.db ctxt) $ \c ->
  do Just (Only (i :: Int)) <- listToMaybe <$> query c "INSERT INTO sets (typ, account_id) VALUES (?, ?) RETURNING id" (Export, account_id)
     void $ execute c "INSERT INTO entry_sets (entry_id, set_id) SELECT id, ? FROM entries WHERE account_id = ?" (i, account_id)

createImport :: Ctxt -> Int -> [Entry] -> IO ()
createImport ctxt account_id entries =
  withResource (Context.db ctxt) $ \c ->
    do Just (Only (set_id :: Int)) <- listToMaybe <$> query c "INSERT INTO sets (typ, account_id) VALUES (?, ?) RETURNING id" (Import, account_id)
       void $ mapM (\e ->
                      do Just (Only (entry_id :: Int)) <- listToMaybe <$> query c "INSERT INTO entries (account_id, who, what, date, howmuch, whopays, category) VALUES (?,?,?,?,?,?,'') RETURNING id" (Entry.accountId e, Entry.whoId e, Entry.description e, Entry.date e, Entry.howmuch e, PGArray $ Entry.whopaysIds e)
                         execute c "INSERT INTO entry_sets (entry_id, set_id) VALUES (?, ?)" (entry_id, set_id)) entries

updateSet :: Bool -> Ctxt -> Int -> Int -> IO ()
updateSet setting ctxt account_id set_id =
  do v <- if setting
             then Just <$> getCurrentTime
             else return Nothing
     withResource (Context.db ctxt) $ \c ->
      void $ execute c "UPDATE entries SET archived = ? WHERE id IN (SELECT entry_id FROM entry_sets WHERE set_id = ?) AND account_id = ?" (v, set_id, account_id)

archiveSet :: Ctxt -> Int -> Int -> IO ()
archiveSet = updateSet True

restoreSet :: Ctxt -> Int -> Int -> IO ()
restoreSet = updateSet False

getForAccount :: Ctxt -> Int -> IO [Set]
getForAccount ctxt account_id =
  withResource (Context.db ctxt) $
    \c -> query c "select S.id, S.typ, S.created_at, ES.count, ES.archived from sets as S join (select set_id, count(entry_id) as count, sum(case when archived is null then 0 else 1 end) as archived from entry_sets join entries on entry_id = id group by set_id) as ES on ES.set_id = S.id where S.account_id = ? ORDER BY S.created_at DESC" (Only account_id)
