{-# LANGUAGE OverloadedStrings #-}
module Site where

import           Configuration.Dotenv              (loadFile)
import           Control.Monad                     (void, when)
import           Data.Default                      (def)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       ((<>))
import           Data.Pool                         (Pool, createPool)
import           Data.Serialize.Text               ()
import           Data.String                       (fromString)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Vault.Lazy                   as Vault
import           Database.PostgreSQL.Simple        (ConnectInfo (..),
                                                    Connection, close,
                                                    connectPostgreSQL)
import           Network.Wai                       (Application, Request,
                                                    Response)
import           Network.Wai.Session               (withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           System.Directory                  (doesFileExist)
import           System.Environment                (lookupEnv)
import           Web.ClientSession                 (initKey, randomKey)
import           Web.Cookie                        (setCookiePath)
import           Web.Fn
import           Web.Heroku                        (parseDatabaseUrl)
import           Web.Larceny


import           Base

import           Handler.Auth
import           Handler.Entry
import           Handler.Home
import           Handler.Set
import           Handler.Settings
import qualified State.Cache                       as Cache


initializer :: IO Ctxt
initializer = do
  envExists <- doesFileExist ".env"
  when envExists $ loadFile False ".env"
  u <- fmap parseDatabaseUrl <$> lookupEnv "DATABASE_URL"
  let ps = fromMaybe [("host", "localhost")
                     ,("port", "5432")
                     ,("user", "housetab_user")
                     ,("password", "111")
                     ,("dbname", "housetab_devel")]
                     u
  pgpool <- createPool (connectPostgreSQL $ T.encodeUtf8 $ T.intercalate " " $ map (\(k,v) -> k <> "=" <> v) ps)
                        close 1 60 20
  lib <- loadTemplates "templates" defaultOverrides
  session <- Vault.newKey
  return (Ctxt defaultFnRequest pgpool lib session)

app :: IO (Ctxt, Application)
app = do ctxt <- initializer
         mbs <- Cache.get' ctxt "session-key"
         let newkey = do (bs, k) <- randomKey
                         Cache.set' ctxt "session-key" bs
                         return k
         k <- case mbs of
                Nothing -> newkey
                Just bs ->
                  case initKey bs of
                    Right k -> return k
                    Left _  -> newkey
         let store = clientsessionStore k
         return (ctxt, withSession store "_session" def {setCookiePath = Just "/"} (sess ctxt) (toWAI ctxt site))

site :: Ctxt -> IO Response
site ctxt =
     route ctxt [path "auth" ==> Handler.Auth.handle
                ,path "entries" ==> Handler.Entry.handle
                ,path "sets" ==> Handler.Set.handle
                ,end ==> Handler.Home.handle
                ,path "settings" ==> Handler.Settings.handle
                ,path "static" ==> staticServe "static"]
                `fallthrough` do r <- render' ctxt "404"
                                 case r of
                                   Just r' -> return r'
                                   Nothing -> notFoundText "Page not found"
