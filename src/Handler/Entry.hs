{-# LANGUAGE OverloadedStrings #-}
module Handler.Entry where

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

import qualified State.Entry
import qualified State.Types.Entry         as Entry

root :: Text
root = "/"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt =
  route ctxt [segment // path "delete" ==> delH]

delH :: Ctxt -> Int -> IO (Maybe Response)
delH ctxt i =
     do mac <- currentAccountId ctxt
        case mac of
          Nothing -> return ()
          Just aid -> State.Entry.delete ctxt aid i
        redirect root
