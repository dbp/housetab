{-# LANGUAGE OverloadedStrings #-}
module Base
       (module C
       , render'
       , renderWith'
       , currentAccountId
       )
       where

import           Control.Monad.Trans (liftIO)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Network.Wai         (Response)
import           Text.Read           (readMaybe)
import           Web.Fn              (okHtml)
import           Web.Larceny         (render, renderWith, subs, textFill')

import           Context
import qualified Context             as C
import qualified State.Account
import qualified State.Types.Account as Account

render' :: Ctxt -> Text -> IO (Maybe Response)
render' ctxt = renderWith' ctxt mempty

renderWith' :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith' ctxt subs tpl =
  do t <- renderWith (library ctxt) (M.union subs (defaultSubs ctxt)) () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'

currentAccountId :: Ctxt -> IO (Maybe Int)
currentAccountId ctxt =
  (>>= readMaybe . T.unpack) <$>
  getFromSession ctxt "account_id"

defaultSubs :: Ctxt -> Substitutions
defaultSubs ctxt =
  subs [("loggedInAccount", textFill' $
         do mt <- liftIO $ currentAccountId ctxt
            case mt of
               Nothing -> return ""
               Just aid ->
                 do a <- liftIO $ State.Account.get ctxt aid
                    case a of
                      Nothing -> return ""
                      Just a' -> return (Account.name a'))]
