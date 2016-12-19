{-# LANGUAGE OverloadedStrings #-}
module Handler.Entry where

import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Format
import           Network.HTTP.Types.Method
import           Network.Wai
import           Web.Fn
import           Web.Larceny               (mapSubs, subs, textFill)
import Web.Fn.Extra.Digestive
import Text.Digestive.Larceny
import Text.Digestive.Form
import Data.Time.Clock
import Data.Time.Calendar

import           Base

import qualified State.Entry
import qualified State.Person
import qualified State.Types.Entry         as Entry
import  State.Types.Entry         (Entry)
import qualified State.Types.Person         as Person
import State.Types.Person (Person)

root :: Text
root = "/"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt =
  route ctxt [segment // path "delete" ==> delH
             ,segment // path "edit" ==> editH]

delH :: Ctxt -> Int -> IO (Maybe Response)
delH ctxt i =
     do mac <- currentAccountId ctxt
        case mac of
          Nothing -> return ()
          Just aid -> State.Entry.delete ctxt aid i
        redirect root

entryForm :: [Person] -> Maybe Entry -> Form Text IO Entry
entryForm people me =
  Entry.Entry
  <$> pure (fromMaybe 0 (Entry.id <$> me))
  <*> pure (fromMaybe 0 (Entry.accountId <$> me))
  <*> "who" .: choice (map (\p -> (Person.id p, Person.name p)) people) (Entry.whoId <$> me)
  <*> "description" .: text (Entry.description <$> me)
  <*> "date" .: ((\d -> UTCTime d 0) <$> dateFormlet "%F" (utctDay <$> (Entry.date <$> me)))
  <*> "howmuch" .: stringRead "Number, like 5.05" (Entry.howmuch <$> me)
  <*> pure (fromMaybe [] (Entry.whopaysIds <$> me))

editH :: Ctxt -> Int -> IO (Maybe Response)
editH ctxt i =
     do mac <- currentAccountId ctxt
        case mac of
          Nothing -> return Nothing
          Just aid ->
            do me <- State.Entry.get ctxt aid i
               people <- State.Person.getForAccount ctxt aid
               case me of
                 Nothing -> redirect root
                 Just entry ->
                   runForm ctxt "edit" (entryForm people (Just entry)) $ \r ->
                     case r of
                       (v, Nothing) -> renderWith' ctxt (formFills v) "entry/edit"
                       (_, Just entry') -> do State.Entry.update ctxt aid entry'
                                              redirect root
