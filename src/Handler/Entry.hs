{-# LANGUAGE OverloadedStrings #-}
module Handler.Entry where

import           Control.Monad (mplus)
import           Control.Logging
import           Control.Monad.Trans       (liftIO)
import           Data.Maybe                (fromJust, fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP.Types.Method
import           Network.Wai
import           Text.Digestive.Form
import           Text.Digestive.Larceny
import           Web.Fn
import           Web.Fn.Extra.Digestive
import           Web.Larceny               (mapSubs, subs, textFill)

import           Base

import qualified State.Entry
import qualified State.Person
import           State.Types.Entry         (Entry)
import qualified State.Types.Entry         as Entry
import           State.Types.Person        (Person)
import qualified State.Types.Person        as Person


root :: Text
root = "/"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt =
  route ctxt [path "add" ==> addH
             ,segment // path "delete" ==> delH
             ,segment // path "edit" ==> editH]

addH :: Ctxt -> IO (Maybe Response)
addH ctxt =
     do mac <- currentAccountId ctxt
        case mac of
          Nothing -> redirect root
          Just aid ->
            do people <- State.Person.getForAccount ctxt aid
               today <- utctDay <$> getCurrentTime
               runForm ctxt "add" (entryForm today people Nothing) $ \r ->
                 case r of
                   (v, Nothing) -> renderWith' ctxt (formFills v) "entry/edit"
                   (_, Just entry') -> do State.Entry.create ctxt aid entry'
                                          redirect root


delH :: Ctxt -> Int -> IO (Maybe Response)
delH ctxt i =
     do mac <- currentAccountId ctxt
        case mac of
          Nothing  -> return ()
          Just aid -> State.Entry.delete ctxt aid i
        redirect root


checkboxForm :: Monad m => Maybe (Int, Text, Bool) -> Form Text m (Int, Text, Bool)
checkboxForm e = ( , , )
        <$> "id" .: stringRead "Internal error." (fst3 <$> e)
        <*> "name" .: text (snd3 <$> e)
        <*> "present" .: bool (trd3 <$> e)

fst3 (a,_,_) = a
snd3 (_,a,_) = a
trd3 (_,_,a) = a

peopleForm :: [Person] -> [Int] -> Form Text IO [Int]
peopleForm people ids = ((map fst3 . filter trd3) <$>
                         listOf
                         (\mp -> checkboxForm (maybe Nothing (\p ->
                                                                Just ( Person.id p
                                                                     , Person.name p
                                                                     , Person.id p `elem` ids)) mp))
                         (Just people))

entryForm :: Day -> [Person] -> Maybe Entry -> Form Text IO Entry
entryForm today people me =
  Entry.Entry
  <$> pure (fromMaybe 0 (Entry.id <$> me))
  <*> pure (fromMaybe 0 (Entry.accountId <$> me))
  <*> "who" .: choice (map (\p -> (Person.id p, Person.name p)) people) (Entry.whoId <$> me)
  <*> "description" .: text (Entry.description <$> me)
  <*> "date" .: ((\d -> UTCTime d 0) <$> dateFormlet "%F" ((utctDay <$> (Entry.date <$> me)) `mplus` (Just today)))
  <*> "howmuch" .: stringRead "Number, like 5.05" (Entry.howmuch <$> me)
  <*> "whopays" .: peopleForm people (fromMaybe (map Person.id people) (Entry.whopaysIds <$> me))

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
                 Just entry -> do
                   today <- utctDay <$> getCurrentTime
                   runForm ctxt "edit" (entryForm today people (Just entry)) $ \r ->
                     case r of
                       (v, Nothing) -> renderWith' ctxt (formFills v) "entry/edit"
                       (_, Just entry') -> do State.Entry.update ctxt aid entry'
                                              redirect root
