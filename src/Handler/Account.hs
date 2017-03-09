{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Account where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.Maybe                (fromJust)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Format
import           Network.HTTP.Types.Method
import           Network.Wai
import           Text.Digestive.Form
import           Text.Digestive.Larceny
import           Text.Digestive.Types      (Result (..))
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Larceny               (fillChildrenWith, mapSubs, subs,
                                            textFill)

import           Base
import qualified Lib

import qualified State.Email
import qualified State.Account
import qualified State.Entry
import qualified State.Person
import qualified State.Share
import qualified State.Types.Account       as Account
import qualified State.Types.Entry         as Entry
import qualified State.Types.Person        as Person
import qualified State.Types.Share         as Share

root :: Text
root = "/account/"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [path "new" ==> newH
                         ,path "welcome" ==> \ctxt -> render' ctxt "account/welcome"]

newForm :: Ctxt -> Form Text IO (Text, Text)
newForm ctxt = (,)
               <$> "account" .: validateM notUsed (text Nothing)
               <*> "email" .: emailDnsCheck ctxt (text Nothing)
  where notUsed a =
          do ma <- State.Account.getByName ctxt a
             case ma of
               Nothing -> return (Success a)
               Just _ -> return (Error "Account name already in use.")

newH :: Ctxt -> IO (Maybe Response)
newH ctxt = runForm ctxt "new" (newForm ctxt) $ \r ->
              case r of
                (v, Nothing) -> renderWith' ctxt (formFills v) "account/new"
                (v, Just (a,e)) -> do ma <- State.Account.create ctxt (Account.Account 0 a)
                                      case ma of
                                        Nothing -> renderWith' ctxt (formFills v) "account/new"
                                        Just ac -> do
                                          State.Email.new ctxt (Account.id ac) e
                                          redirect (root <> "welcome")

