{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Home where

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
import           Web.Larceny               (fillChildrenWith, mapSubs, subs,
                                            textFill, fillChildren)

import           Base
import qualified Lib

import qualified State.Account
import qualified State.Entry
import qualified State.Person
import qualified State.Share
import qualified State.Types.Account       as Account
import qualified State.Types.Entry         as Entry
import qualified State.Types.Person        as Person
import qualified State.Types.Share         as Share

root :: Text
root = "/"

handle :: Ctxt -> Maybe Int -> IO (Maybe Response)
handle ctxt pg' =
  do let pg = fromMaybe 1 pg'
     ma <- currentAccountId ctxt
     s <-
       case ma of
         Nothing -> return $ subs [("entries", textFill "")]
         Just acnt_id ->
           do es <- State.Entry.getForAccount ctxt acnt_id
              ps <- State.Person.getForAccount ctxt acnt_id
              ps_ws <- mapM (\p -> (p, ) <$> State.Share.getForPerson ctxt (Person.id p)) ps
              let (Lib.Result people today) =
                    Lib.run ps_ws es
              let es_show = take 100 $ drop (100 * (pg - 1)) es
              return $ subs [("entries", mapSubs (entrySubs ps) es_show)
                            ,("results", mapSubs resultSubs people)
                            ,("pages", pagesSubs pg (ceiling $ fromIntegral (length es) / 100) )]
     renderWith' ctxt s "index"

pagesSubs :: Int -> Int -> Fill
pagesSubs pg n = mapSubs (\n -> subs [("page", textFill (tshow n))
                                  ,("is-current", if n == pg then fillChildren else textFill "")
                                  ,("not-current", if n == pg then textFill "" else fillChildren)])
                      (take n [1..])

moneyShow :: Double -> Text
moneyShow d =
  let dollars' = floor d :: Int
      dollars = if dollars' > 0
                   then "$" <> tshow dollars'
                   else "-$" <> tshow (-dollars')
      cents' = floor $ 100 * (d - fromIntegral dollars')
      cents = if cents' < 10
                 then "0" <> tshow cents'
                 else tshow cents'
  in dollars <> "." <> cents

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
       ,("howmuch", textFill (moneyShow hm))
       ,("whopays", mapSubs (\(isl, pi) -> subs [("id", textFill (tshow pi))
                                                ,("name", textFill (Person.name (getP pi)))
                                                ,("sep", textFill $ if isl
                                                                       then ""
                                                                       else ",")])
                              (zip (replicate (length wps - 1) False ++ repeat True) wps))]
  where getP i = fromJust $ lookup i (map (\p -> (Person.id p, p)) ps)

personSubs :: Person.Person -> Substitutions
personSubs p = subs [("id", textFill $ tshow $ Person.id p)
                    ,("name", textFill $ Person.name p)]

resultSubs :: (Person.Person, Lib.Spent, Lib.Owes)
           -> Substitutions
resultSubs (p, s, o) =
      subs [("person", fillChildrenWith (personSubs p))
           ,("spent", textFill (moneyShow s))
           ,("owes", textFill (moneyShow o))]
