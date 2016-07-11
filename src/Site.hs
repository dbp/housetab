{-# LANGUAGE OverloadedStrings #-}
module Site where

import           Configuration.Dotenv              (loadFile)
import           Control.Monad                     (when)
import           Data.Default                      (def)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       ((<>))
import           Data.Pool                         (Pool, createPool)
import           Data.Serialize.Text               ()
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Vault.Lazy                   as Vault
import           Database.PostgreSQL.Simple        (ConnectInfo (..),
                                                    Connection, close,
                                                    connectPostgreSQL)
import           Larceny
import           Network.Wai                       (Application, Request,
                                                    Response)
import           Network.Wai.Session               (withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           System.Directory                  (doesFileExist)
import           System.Environment                (lookupEnv)
import           Web.ClientSession                 (randomKey)
import           Web.Cookie                        (setCookiePath)
import           Web.Fn
import           Web.Heroku                        (parseDatabaseUrl)


import           Context

import           Handler.Auth
import           Handler.Home

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
  lib <- loadTemplates "templates"
  session <- Vault.newKey
  return (Ctxt defaultFnRequest pgpool lib session)

app :: IO Application
app = do (_, k) <- randomKey
         let store = clientsessionStore k
         ctxt <- initializer
         return (withSession store "_session" def {setCookiePath = Just "/"} (sess ctxt) (toWAI ctxt site))

site :: Ctxt -> IO Response
site ctxt = route ctxt [path "auth" ==> Handler.Auth.handle
                       ,end ==> Handler.Home.handle]
                  `fallthrough` notFoundText "Page Not Found"
