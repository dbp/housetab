{-# LANGUAGE OverloadedStrings #-}

module State.Auth where

import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Account
import           State.Types.Authentication

create :: Ctxt -> Text -> IO (Maybe Authentication)
create ctxt email = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "INSERT INTO authentications (account_id, email_id) (SELECT account_id, id FROM emails WHERE email = ?) RETURNING id, account_id, email_id, created_at, token" (Only email)

check :: Ctxt -> Text -> IO (Maybe Account)
check ctxt token = withResource (Context.db ctxt) $ \c -> listToMaybe <$> query c "SELECT A.id, A.name FROM accounts AS A JOIN authentications AS T on T.account_id = A.id WHERE T.token = ? and T.created_at + interval '1 day' > now()" (Only token)
