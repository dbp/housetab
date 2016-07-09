{-# LANGUAGE OverloadedStrings #-}
module Site where

import           Configuration.Dotenv       (loadFile)
import           Control.Monad              (when)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool, createPool)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connectPostgreSQL)
import           Larceny
import           Network.Wai                (Application, Request, Response)
import           System.Directory           (doesFileExist)
import           System.Environment         (lookupEnv)
import           Web.Fn
import           Web.Heroku                 (parseDatabaseUrl)

data Ctxt = Ctxt FnRequest (Pool Connection) (Library ())

instance RequestContext Ctxt where
  getRequest (Ctxt r _ _) = r
  setRequest (Ctxt _ p l) r = Ctxt r p l

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
  return (Ctxt defaultFnRequest pgpool lib)

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site

site :: Ctxt -> IO Response
site ctxt = route ctxt [end ==> \_ -> okText "Hello"]
                  `fallthrough` notFoundText "Page Not Found"
