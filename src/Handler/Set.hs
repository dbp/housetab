{-# LANGUAGE OverloadedStrings #-}
module Handler.Set where

import           Base
import           Context
import qualified Data.Csv                  as CSV
import           Data.List                 (find)
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy.Encoding   as T
import qualified Data.Text.Lazy.IO         as T
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import qualified Data.Vector               as V
import           Network.HTTP.Types.Status (status200)
import           Network.Wai
import           Text.Digestive.Form
import           Text.Digestive.Types      (Result (..))
import           Web.Fn
import           Web.Larceny               (mapSubs, subs, textFill)

import qualified State.Person
import qualified State.Set
import           State.Types.Entry         (Entry)
import qualified State.Types.Entry         as Entry
import           State.Types.Person        (Person)
import qualified State.Types.Person        as Person
import           State.Types.Set
import           Text.Digestive.Larceny
import           Web.Fn.Extra.Digestive

root :: Text
root = "/"

handle :: Ctxt -> IO (Maybe Response)
handle ctxt = do mid <- currentAccountId ctxt
                 case mid of
                   Nothing -> redirect root
                   Just aid ->
                     route ctxt [path "export" ==> exportH aid
                                ,path "import" ==> importH aid
                                ,end ==> indexH aid
                                ,segment // path "download" ==> downloadH aid
                                ,segment // path "archive" ==> archiveH aid
                                ,segment // path "restore" ==> restoreH aid]

exportH :: Int -> Ctxt -> IO (Maybe Response)
exportH aid ctxt = do State.Set.createExport ctxt aid
                      redirect "/sets"

importForm :: Form Text IO [ExportEntry]
importForm = "file" .: validateM c (Text.Digestive.Form.file)
  where c Nothing  = return $ Error "File missing"
        c (Just p) =
          do csv <- T.readFile p
             T.putStrLn csv
             case CSV.decodeByName (T.encodeUtf8 csv) of
               Left err      -> return $ Error (T.pack err)
               Right (_, vs) -> return $ Success (V.toList vs)

importH :: Int -> Ctxt -> IO (Maybe Response)
importH aid ctxt =
  do people <- State.Person.getForAccount ctxt aid
     runForm ctxt "import" importForm $ \r ->
       case r of
         (v, Nothing)      -> renderWith' ctxt (formFills v) "set/import"
         (_, Just entries) ->
           do let es = unresolvePeople aid people entries
              State.Set.createImport ctxt aid es
              redirect "/sets"

indexH :: Int -> Ctxt -> IO (Maybe Response)
indexH aid ctxt =
  do sets <- State.Set.getForAccount ctxt aid
     renderWith' ctxt (subs [("sets", mapSubs setSubs sets)]) "set/index"

setSubs :: Set -> Substitutions
setSubs (Set i t cr cnt arch) =
  subs [("id", textFill $ tshow i)
       ,("type", textFill $ tshow t)
       ,("created-at", textFill $ T.pack $ formatTime defaultTimeLocale "%F" cr)
       ,("count", textFill $ tshow cnt)
       ,("archived", textFill $ tshow arch)]


downloadH :: Int -> Ctxt -> Int -> IO (Maybe Response)
downloadH aid ctxt i =
  do entries <- State.Set.getEntries ctxt aid i
     people <- State.Person.getForAccount ctxt aid
     let ees = resolvePeople people entries
     return (Just (responseLBS status200 [("Content-type", "text/csv")]
                                         (CSV.encodeDefaultOrderedByName ees)))

resolvePeople :: [Person] -> [Entry] -> [ExportEntry]
resolvePeople people es = map (\e -> ExportEntry (Entry.id e)
                                                 (name (Entry.whoId e))
                                                 (Entry.description e)
                                                 (Entry.date e)
                                                 (Entry.howmuch e)
                                                 (map name (Entry.whopaysIds e))) es
  where name i = Person.name (fromJust (find (\p -> Person.id p == i) people) )

unresolvePeople :: Int -> [Person] -> [ExportEntry] -> [Entry]
unresolvePeople aid people es = map (\e -> Entry.Entry (eid e)
                                                       aid
                                                       (lookup (who e))
                                                       (description e)
                                                       (date e)
                                                       (howmuch e)
                                                       (map lookup (whopays e)))
                                    es
  where lookup n = Person.id (fromJust (find (\p -> Person.name p == n) people))

archiveH :: Int -> Ctxt -> Int -> IO (Maybe Response)
archiveH aid ctxt sid = do State.Set.archiveSet ctxt aid sid
                           redirect "/sets"

restoreH :: Int -> Ctxt -> Int -> IO (Maybe Response)
restoreH aid ctxt sid = do State.Set.restoreSet ctxt aid sid
                           redirect "/sets"
