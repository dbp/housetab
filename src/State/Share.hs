{-# LANGUAGE OverloadedStrings #-}

module State.Share where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Share

create :: Ctxt -> Share -> IO ()
create ctxt share = withResource (Context.db ctxt) $ \c -> void $ execute c "INSERT INTO shares (person_id, start, value) VALUES (?,?,?)" (personId share, start share, value share)

getForPerson :: Ctxt -> Int -> IO [Share]
getForPerson ctxt person_id =
  withResource (Context.db ctxt) $ \c -> query c "SELECT id, person_id, start, value FROM shares WHERE person_id = ? ORDER BY start ASC" (Only person_id)

get :: Ctxt -> Int -> Int -> IO (Maybe Share)
get ctxt account_id id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT S.id, S.person_id, S.start, S.value FROM shares AS S JOIN persons as P on P.id = S.person_id WHERE P.account_id = ? AND S.id = ?" (account_id, id')

delete :: Ctxt -> Int -> Int -> IO ()
delete ctxt account_id share_id = withResource (Context.db ctxt) $ \c -> void $ execute c "DELETE FROM shares WHERE id IN (SELECT S.id FROM shares AS S JOIN persons as P on P.id = S.person_id where P.account_id = ? AND S.id = ?)" (account_id, share_id)
