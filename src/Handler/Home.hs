{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Network.HTTP.Types.Method
import           Network.Wai
import           Web.Fn
import           Web.Larceny               (mapSubs, subs, textFill)

import           Base

import qualified State.Account
import qualified State.Entry
import qualified State.Types.Account       as Account
import qualified State.Types.Entry         as Entry

root :: Text
root = "/"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt =
  do ma <- currentAccountId ctxt
     s <-
       case ma of
         Nothing -> return $ subs [("entries", textFill "")]
         Just acnt_id ->
           do es <- State.Entry.getForAccount ctxt acnt_id
              return $ subs [("entries", mapSubs entrySubs es)]
     renderWith' ctxt s "index"

entrySubs :: Entry.Entry -> Substitutions
entrySubs (Entry.Entry i a w desc dt hm wps) =
  subs [("id", textFill (tshow i))
       ,("account-id", textFill (tshow a))
       ,("who-id", textFill (tshow w))
       ,("description", textFill desc)
       ,("date", textFill (tshow dt))
       ,("howmuch", textFill (tshow hm))
       ,("whopays", mapSubs (\pi -> subs [("id", textFill (tshow pi))])
                            wps)]
