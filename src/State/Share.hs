{-# LANGUAGE OverloadedStrings #-}

module State.Share where

import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Share

getForPerson :: Ctxt -> Int -> IO [Share]
getForPerson ctxt person_id =
  withResource (Context.db ctxt) $ \c -> query c "SELECT id, person_id, start, value FROM shares WHERE person_id = ? ORDER BY start ASC" (Only person_id)
