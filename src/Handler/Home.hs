{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.Maybe                (fromJust)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Format
import           Network.HTTP.Types.Method
import           Network.Wai
import           Web.Fn
import           Web.Larceny               (mapSubs, subs, textFill)

import           Base

import qualified State.Account
import qualified State.Entry
import qualified State.Person
import qualified State.Types.Account       as Account
import qualified State.Types.Entry         as Entry
import qualified State.Types.Person        as Person

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
              ps <- State.Person.getForAccount ctxt acnt_id
              return $ subs [("entries", mapSubs (entrySubs ps) es)]
     renderWith' ctxt s "index"

entrySubs :: [Person.Person] -> Entry.Entry -> Substitutions
entrySubs ps (Entry.Entry i a w desc dt hm wps) =
  subs [("id", textFill (tshow i))
       ,("account-id", textFill (tshow a))
       ,("who", textFill (Person.name (getP w)))
       ,("description", textFill desc)
       ,("date", textFill
                   (T.pack $ formatTime
                               defaultTimeLocale
                               "%Y-%m-%d"
                               dt))
       ,("howmuch", textFill (tshow hm))
       ,("whopays", mapSubs (\(isl, pi) -> subs [("id", textFill (tshow pi))
                                                ,("name", textFill (Person.name (getP pi)))
                                                ,("sep", textFill $ if isl
                                                                       then ""
                                                                       else ",")])
                              (zip (replicate (length wps - 1) False ++ repeat True) wps))]
  where getP i = fromJust $ lookup i (map (\p -> (Person.id p, p)) ps)
