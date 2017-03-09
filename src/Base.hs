{-# LANGUAGE OverloadedStrings #-}
module Base
       (module C
       , render'
       , renderWith'
       , renderText'
       , currentAccountId
       , emailDnsCheck
       )
       where

import           Control.Monad.Trans  (liftIO)
import qualified Data.Map             as M
import           Data.Maybe           (isJust, isNothing)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Network.DNS.Lookup
import           Network.DNS.Resolver
import           Network.DNS.Types
import           Network.Wai          (Response)
import           Text.Digestive.Form
import           Text.Digestive.Types (Result (..))
import           Text.Read            (readMaybe)
import           Web.Fn               (okHtml, okText)
import           Web.Larceny          (Fill (..), fillChildren, render,
                                       renderWith, subs, textFill, textFill')

import           Context
import qualified Context              as C
import qualified State.Account
import qualified State.Types.Account  as Account

render' :: Ctxt -> Text -> IO (Maybe Response)
render' ctxt = renderWith' ctxt mempty

renderWith' :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith' ctxt subs tpl =
  do t <- renderWith (library ctxt) (M.union subs (defaultSubs ctxt)) () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'

renderText' :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderText' ctxt subs tpl =
  do t <- renderWith (library ctxt) (M.union subs (defaultSubs ctxt)) () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okText t'

currentAccountId :: Ctxt -> IO (Maybe Int)
currentAccountId ctxt =
  (>>= readMaybe . T.unpack) <$>
  getFromSession ctxt "account_id"

ifFill mb = Fill $ \x y z ->
  do mt <- liftIO mb
     if mt
        then unFill fillChildren x y z
        else unFill (textFill "") x y z

defaultSubs :: Ctxt -> Substitutions
defaultSubs ctxt =
  subs [("isLoggedIn", ifFill (isJust <$> currentAccountId ctxt))
       ,("notLoggedIn", ifFill (isNothing <$> currentAccountId ctxt))
       ,("loggedInAccount", textFill' $
         do mt <- liftIO $ currentAccountId ctxt
            case mt of
               Nothing -> return ""
               Just aid ->
                 do a <- liftIO $ State.Account.get ctxt aid
                    case a of
                      Nothing -> return ""
                      Just a' -> return (Account.name a'))]

emailDnsCheck :: Ctxt -> Form Text IO Text
                      -> Form Text IO Text
emailDnsCheck ctxt = checkM "Email address domain (after the @) not valid." $ \e ->
                       do let host = T.encodeUtf8 (T.drop 1 (snd (T.breakOn "@" e)))
                          res <- withResolver (dns ctxt) (`lookupMX` host)
                          case res of
                            Left err -> case err of
                                          IllegalDomain -> return False
                                          NameError     -> return False
                                          _             -> return True
                            Right [] -> return False
                            _ -> return True
