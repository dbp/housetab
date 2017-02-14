{-# LANGUAGE OverloadedStrings #-}
module Handler.Auth where

import           Control.Lens               hiding (use)
import           Control.Logging
import           Data.Maybe
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Network.AWS                hiding (Response)
import           Network.AWS.SES
import           Network.HTTP.Types.Method
import           Network.Wai
import           System.Environment
import           System.IO                  (stdout)
import           Web.Fn
import           Web.Larceny

import           Base
import qualified State.Account
import qualified State.Auth                 as State
import qualified State.Email
import qualified State.Types.Account        as Account
import qualified State.Types.Authentication as Authentication
import qualified State.Types.Email          as Email

root :: Text
root = "/auth"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [path "new" // method GET ==> new
                         ,path "new" // param "account" // param "email" // method POST !=> create
                         ,path "new" // param "account" // method POST !=> select
                         ,path "use" // param "token" ==> use
                         ,path "destroy" ==> destroy]

new :: Ctxt -> IO (Maybe Response)
new ctxt = render' ctxt "auth/new"

select :: Ctxt -> Text -> IO (Maybe Response)
select ctxt account =
  do ems <- State.Email.getVerifiedEmails ctxt account
     renderWith' ctxt (subs [("account", textFill account)
                            ,("emails", mapSubs (\e -> subs [("id", textFill (tshow (Email.id e)))
                                                            ,("email", textFill $ obfuscate (Email.email e))]) ems)])
                      "auth/select"
  where obfuscate e = let [b,a] = T.splitOn "@" e
                          blen = T.length b
                          btk = if blen > 3
                                   then 3
                                   else 2
                          newb = T.take btk b <> T.replicate (blen - btk) "*"
                          alen = T.length a
                          atk = if alen > 3
                                   then 3
                                   else 2
                          newa = T.replicate atk "*" <> T.drop atk a
                      in newb <> "@" <> newa


create :: Ctxt -> Text -> Int -> IO (Maybe Response)
create ctxt account email_id =
  do a <- State.create ctxt account email_id
     e' <- State.Email.getEmailById ctxt email_id
     case (a,e') of
       (Just a', Just em) ->
         do lgr  <- newLogger Debug stdout
            env  <- newEnv NorthVirginia Discover
            domain <- fromMaybe "http://localhost:8000" <$> lookupEnv "DOMAIN"
            let msg = "To log in to your HouseTab account, please visit the following link:\n\n" <> T.pack domain <> root <> "/use?token=" <> Authentication.token a' <> "\n\nThanks!\n\nP.S. If you did not try to log in, feel free to ignore this message."
            runResourceT $ runAWS (env & envLogger .~ lgr) $
                    send (sendEmail "info@housetab.org"
                                    (destination & dToAddresses .~ [Email.email em])
                                    (message (content "Housetab :: Log in to your account")
                                             (body & bText .~ (Just (content msg))))
                                    )
            render' ctxt "auth/create"
       _ -> redirect $ root <> "/new"

use :: Ctxt -> Text -> IO (Maybe Response)
use ctxt token =
  do a <- State.check ctxt token
     case a of
       Nothing -> render' ctxt "auth/invalid"
       Just a' -> do setInSession ctxt
                                  "account_id"
                                  (tshow $ Account.id a')
                     redirect "/"

destroy :: Ctxt -> IO (Maybe Response)
destroy ctxt = do clearFromSession ctxt "account_id"
                  redirect "/"
