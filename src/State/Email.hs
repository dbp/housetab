{-# LANGUAGE OverloadedStrings #-}

module State.Email where

import           Control.Monad              (void)
import           Data.Maybe
import           Data.Pool
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Context
import           State.Types.Email

new :: Ctxt -> Int -> Text -> IO (Maybe Email)
new ctxt i em = listToMaybe <$> (withResource (Context.db ctxt) $ \c -> query c "INSERT INTO emails (account_id, email) VALUES (?,?) RETURNING id, account_id, email, verified_at, token" (i,em))

verify :: Ctxt -> Int -> Text -> IO ()
verify ctxt i tok = withResource (Context.db ctxt) $ \c -> void $ execute c "UPDATE emails SET verified_at = now() WHERE id = ? AND token = ?" (i,tok)

delete :: Ctxt -> Int -> Int -> IO ()
delete ctxt aid emi = withResource (Context.db ctxt) $ \c -> void $ execute c "DELETE FROM authentications WHERE email_id = ?; DELETE FROM emails WHERE id = ? AND account_id = ?" (emi, emi, aid)

getEmails :: Ctxt -> Text -> IO [Email]
getEmails ctxt name = withResource (Context.db ctxt) $ \c -> query c "SELECT E.id, E.account_id, E.email, E.verified_at, E.token FROM emails as E join accounts as A on A.id = E.account_id WHERE A.name = ?" (Only name)

getVerifiedEmails :: Ctxt -> Text -> IO [Email]
getVerifiedEmails ctxt name = withResource (Context.db ctxt) $ \c -> query c "SELECT E.id, E.account_id, E.email, E.verified_at, E.token FROM emails as E join accounts as A on A.id = E.account_id WHERE A.name = ? AND E.verified_at IS NOT NULL" (Only name)

getEmailsByAccountId :: Ctxt -> Int -> IO [Email]
getEmailsByAccountId ctxt aid = withResource (Context.db ctxt) $ \c -> query c "SELECT E.id, E.account_id, E.email, E.verified_at, E.token FROM emails as E join accounts as A on A.id = E.account_id WHERE A.id = ?" (Only aid)

getEmailById :: Ctxt -> Int -> IO (Maybe Email)
getEmailById ctxt i = listToMaybe <$> (withResource (Context.db ctxt) $ \c -> query c "SELECT E.id, E.account_id, E.email, E.verified_at, E.token FROM emails as E WHERE E.id = ?" (Only i))
