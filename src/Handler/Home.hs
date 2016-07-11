{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Larceny
import           Network.HTTP.Types.Method
import           Network.Wai
import           Web.Fn

import           Context

import qualified State.Account
import qualified State.Types.Account       as Account

root :: Text
root = "/"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = renderWith' ctxt (subs [("loggedInAccount", \_ _ _ ->
                                        do mt <- liftIO $ getFromSession ctxt "account_id"
                                           case mt of
                                              Nothing -> return ""
                                              Just aid -> do a <- liftIO $ State.Account.get ctxt (read $ T.unpack aid)
                                                             case a of
                                                               Nothing -> return ""
                                                               Just a' -> return (Account.name a'))]) "index"
