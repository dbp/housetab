{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Settings where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Data.Time.Format
import           Network.AWS        hiding (Response)
import           Network.AWS.SES
import           Network.Wai
import           System.Environment
import           System.IO          (stdout)
import           Web.Fn
import           Web.Larceny        (fillChildren, fillChildrenWith, mapSubs,
                                     subs, textFill)

import           Base

import qualified State.Email
import qualified State.Entry
import qualified State.Person
import qualified State.Share
import           State.Types.Email  (Email)
import qualified State.Types.Email  as Email
import           State.Types.Person (Person)
import qualified State.Types.Person as Person
import           State.Types.Share  (Share)
import qualified State.Types.Share  as Share
import qualified State.Types.Account  as Account


root :: Text
root = "/settings"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = route ctxt [end ==> indexH
                         ,path "email" // path "new" // param "email" ==> newEmailH
                         ,path "email" // segment // path "delete" ==> deleteEmailH
                         ,path "email" // segment // path "verify" ==> verifyEmailH
                         ,path "person" // path "new" // param "name" ==> newPersonH
                         ,path "person" // segment // path "delete" ==> deletePersonH
                         ,path "share" // path "new" // param "person" // param "date" // param "value" ==> newShareH
                         ,path "share" // segment ==> shareH]

personShareSubs :: (Person, [Share]) -> Substitutions
personShareSubs (p,ss) =
  subs [("id", textFill $ tshow $ Person.id p)
       ,("name", textFill $ Person.name p)
       ,("shares", mapSubs (\s -> subs [("id", textFill $ tshow $ Share.id s), ("start", textFill $ (T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (Share.start s))), ("value", textFill $ tshow $ Share.value s)]) ss)]

emailSubs :: Email -> Substitutions
emailSubs e = subs [("id", textFill $ tshow $ Email.id e)
                   ,("email", textFill $ Email.email e)
                   ,("not-verified", case Email.verifiedAt e of
                                       Nothing -> fillChildren
                                       Just _  -> textFill "")
                   ,("verified", textFill $ case Email.verifiedAt e of
                                              Nothing -> "no"
                                              Just _  -> "yes")]

indexH :: Ctxt -> IO (Maybe Response)
indexH ctxt =
  do mac <- currentAccountId ctxt
     case mac of
       Nothing -> redirect "/"
       Just aid ->
         do _people <- State.Person.getForAccount ctxt aid
            people <- mapM (\p -> (p,) <$> State.Share.getForPerson ctxt (Person.id p)) _people
            now <- getCurrentTime
            emails <- State.Email.getEmailsByAccountId ctxt aid
            let s = subs [("people", mapSubs personShareSubs people)
                         ,("now", textFill $ (T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" now))
                         ,("emails", mapSubs emailSubs emails)]
            renderWith' ctxt s "settings"

newEmailH :: Ctxt -> Text -> IO (Maybe Response)
newEmailH ctxt email =
  do mac <- currentAccountId ctxt
     case mac of
       Nothing -> redirect "/"
       Just aid ->
         do State.Email.new ctxt aid email
            redirect root

deleteEmailH :: Ctxt -> Int -> IO (Maybe Response)
deleteEmailH ctxt i =
  do mac <- currentAccountId ctxt
     case mac of
       Nothing -> redirect "/"
       Just aid ->
         do ems <- State.Email.getEmailsByAccountId ctxt aid
            if length ems > 1
               then case filter (\e -> Email.id e == i) ems of
                      [] -> redirect root
                      (x:_) -> do State.Email.delete ctxt aid (Email.id x)
                                  redirect root
               else redirect root


data EmailVerify = EmailVerify Int Text

instance FromParam EmailVerify where
  fromParam [x] = case T.splitOn "-" x of
                    [i',t] -> case fromParam [i'] of
                                Left e  -> Left e
                                Right i -> Right (EmailVerify i t)
                    _ -> Left ParamUnparsable
  fromParam []  = Left ParamMissing
  fromParam _   = Left ParamTooMany

verifyEmailH :: Ctxt -> EmailVerify -> IO (Maybe Response)
verifyEmailH ctxt (EmailVerify i t) =
  do ma <- State.Email.verify ctxt i t
     case ma of
       Nothing -> return ()
       Just a -> setInSession ctxt "account_id" (tshow $ Account.id a) 
     redirect root

newPersonH :: Ctxt -> Text -> IO (Maybe Response)
newPersonH ctxt name =
  do mac <- currentAccountId ctxt
     case mac of
       Nothing -> redirect root
       Just aid -> do State.Person.create ctxt (Person.Person 0 aid name)
                      redirect root

deletePersonH :: Ctxt -> Int -> IO (Maybe Response)
deletePersonH ctxt pid =
  do mac <- currentAccountId ctxt
     case mac of
       Nothing -> redirect root
       Just aid ->
         do people <- State.Person.getForAccount ctxt aid
            case filter (\p -> Person.id p == pid) people of
              (p:_) ->
                do es <- State.Entry.getForPerson ctxt p
                   if length es == 0
                      then do State.Person.delete ctxt p
                              redirect root
                      else redirect root
              _ -> redirect root

newShareH :: Ctxt -> Int -> Text -> Double -> IO (Maybe Response)
newShareH ctxt pid dt val =
  do mac <- currentAccountId ctxt
     case mac of
       Nothing -> redirect root
       Just aid ->
         do let mdate = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dt)
            people <- State.Person.getForAccount ctxt aid
            case (mdate, pid `elem` (map Person.id people)) of
              (Just date, True) ->
                 do State.Share.create ctxt (Share.Share 0 pid date val)
                    redirect root
              _ -> redirect root


shareH :: Ctxt -> Int -> IO (Maybe Response)
shareH ctxt id =
  do mac <- currentAccountId ctxt
     case mac of
       Nothing -> redirect "/"
       Just aid ->
         do mp <- State.Share.get ctxt aid id
            case mp of
              Nothing -> redirect root
              Just p ->
                do route ctxt [path "delete" ==> deleteShareH aid id]

deleteShareH :: Int -> Int -> Ctxt -> IO (Maybe Response)
deleteShareH aid sid ctxt =
  do State.Share.delete ctxt aid sid
     redirect root
