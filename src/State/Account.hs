{-# LANGUAGE OverloadedStrings #-}

module State.Account where

import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Account
import           State.Types.Email

get :: Ctxt -> Int -> IO (Maybe Account)
get ctxt id' = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT id, name FROM accounts WHERE id = ?" (Only id')

getEmails :: Ctxt -> Text -> IO [Email]
getEmails ctxt name = withResource (Context.db ctxt) $ \c -> query c "SELECT E.id, E.account_id, E.email, E.verified_at, E.token FROM emails as E join accounts as A on A.id = E.account_id WHERE A.name = ?" (Only name)

getEmailById :: Ctxt -> Int -> IO (Maybe Email)
getEmailById ctxt i = listToMaybe <$> (withResource (Context.db ctxt) $ \c -> query c "SELECT E.id, E.account_id, E.email, E.verified_at, E.token FROM emails as E WHERE E.id = ?" (Only i))
