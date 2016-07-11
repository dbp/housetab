{-# LANGUAGE OverloadedStrings #-}
module Handler.Auth where

import           Control.Logging
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Larceny
import           Network.HTTP.Types.Method
import           Network.Wai
import           Web.Fn

import           Context
import qualified State.Auth                 as State
import qualified State.Types.Account        as Account
import qualified State.Types.Authentication as Authentication

root :: Text
root = "/auth"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [path "new" // method GET ==> new
                         ,path "new" // param "email" // method POST !=> create
                         ,path "use" // param "token" ==> use
                         ,path "destroy" ==> destroy]

new :: Ctxt -> IO (Maybe Response)
new ctxt = render' ctxt "auth/new"

create :: Ctxt -> Text -> IO (Maybe Response)
create ctxt email =
  do a <- State.create ctxt email
     case a of
       Nothing  -> redirect $ root <> "/new"
       Just a' ->
         do -- TODO(dbp 2016-07-10): send this in email
            log' $ root <> "/use?token=" <> Authentication.token a'
            render' ctxt "auth/create"

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
