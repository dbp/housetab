{-# LANGUAGE OverloadedStrings #-}

module State.Email where

import           Control.Lens
import           Control.Monad              (void)
import           Data.Maybe
import           Data.Monoid
import           Data.Pool
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple
import           Network.AWS                hiding (Response)
import           Network.AWS.SES
import           System.Environment
import           System.IO          (stdout)

import           Context
import           State.Types.Email
import           State.Types.Account

new :: Ctxt -> Int -> Text -> IO (Maybe Email)
new ctxt i em = do
  me <- listToMaybe <$> (withResource (Context.db ctxt) $ \c -> query c "INSERT INTO emails (account_id, email) VALUES (?,?) RETURNING id, account_id, email, verified_at, token" (i,em))
  case me of
    Nothing -> return Nothing
    Just e -> do lgr  <- newLogger Debug stdout
                 env  <- newEnv NorthVirginia Discover
                 domain <- fromMaybe "http://localhost:8000" <$> lookupEnv "DOMAIN"
                 let msg = "To confirm this email address for your HouseTab account, please visit the following link:\n\n" <> T.pack domain <> "/settings/email/" <> tshow (State.Types.Email.id e) <> "-" <> token e <> "/verify\n\nThanks!\n\nP.S. If you do not recognize this, feel free to ignore this message."
                 runResourceT $ runAWS (env & envLogger .~ lgr) $
                        send (sendEmail "info@housetab.org"
                                            (destination & dToAddresses .~ [email e])
                                                (message (content "Housetab :: Confirm email address")
                                                             (body & bText .~ (Just (content msg))))
                                                     )
                 return (Just e)


verify :: Ctxt -> Int -> Text -> IO (Maybe Account)
verify ctxt i tok = withResource (Context.db ctxt) $ \c ->
  do void $ execute c "UPDATE emails SET verified_at = now() WHERE id = ? AND token = ?" (i,tok)
     listToMaybe <$> query c "SELECT A.id, A.name FROM accounts AS A JOIN emails AS E ON E.account_id = A.id WHERE E.id = ? AND E.token = ?" (i,tok)



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
