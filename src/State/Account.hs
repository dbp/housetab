{-# LANGUAGE OverloadedStrings #-}

module State.Account where

import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Account

get :: Ctxt -> Int -> IO (Maybe Account)
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, name FROM accounts WHERE id = ?" (Only id')
