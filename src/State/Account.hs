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

getByName :: Ctxt -> Text -> IO (Maybe Account)
getByName ctxt name = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, name FROM accounts WHERE name = ?" (Only name)

create :: Ctxt -> Account -> IO (Maybe Account)
create ctxt account = withResource (Context.db ctxt) $ \c -> listToMaybe <$>
  query c "INSERT INTO accounts (name) VALUES (?) RETURNING id, name" (Only $ name account)
