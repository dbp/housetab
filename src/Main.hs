{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Configuration.Dotenv              (loadFile)
import           Context
import           Control.Exception                 (SomeException (..), catch)
import           Control.Logging                   (log', withStdoutLogging)
import           Control.Monad                     (void, when)
import           Data.List                         (isSuffixOf)
import           Data.Monoid                       ((<>))
import           Data.Pool                         (withResource)
import           Data.String                       (fromString)
import           Data.Text                         (Text, pack)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           Database.PostgreSQL.Simple        (ConnectInfo (..),
                                                    Connection, Only (..),
                                                    close, connectPostgreSQL,
                                                    execute, execute_, query)
import qualified Database.Rivet                    as Rivet
import qualified Database.Rivet.Adaptor.PostgreSQL as Rivet
import qualified Database.Rivet.Main               as Rivet
import           Database.Rivet.V0                 (sql)
import           Network.Wai                       (Middleware, pathInfo)
import           Network.Wai.Handler.Warp          (run)
import           Site                              (app)
import           System.Directory                  (doesFileExist,
                                                    listDirectory)
import           System.Environment                (lookupEnv)
import GHC.IO.Encoding

-- TODO(dbp 2017-02-05): Upstream this into rivet-adaptor-postgresql.
setupConn :: Monad m
          => (m () -> IO ())
          -> Connection
          -> IO (Rivet.Adaptor m)
setupConn h conn =
  do execute_ conn "CREATE TABLE IF NOT EXISTS migrations (name text NOT NULL\
                   \ PRIMARY KEY, run_at timestamptz NOT NULL DEFAULT now())"
     return $ Rivet.Adaptor h (sql conn) (check conn) (mark conn)
  where sql :: Connection -> Text -> IO ()
        sql conn = void . execute_ conn . fromString . T.unpack
        check :: Connection -> Text -> IO Bool
        check conn name =
          not . null <$> (query conn
                                "SELECT name FROM migrations WHERE name = ?"
                                (Only name) :: IO [Only Text])
        mark :: Connection -> Text -> Rivet.Direction -> IO ()
        mark conn name dir =
          case dir of
            Rivet.Up ->
              void $ execute conn
                             "INSERT INTO migrations (name) values (?)"
                             (Only name)
            Rivet.Down ->
              void $ execute conn
                             "DELETE FROM migrations WHERE name = ?"
                             (Only name)

main :: IO ()
main = withStdoutLogging $
       do setLocaleEncoding utf8
          envExists <- doesFileExist ".env"
          when envExists $ loadFile False ".env"
          port <- maybe 8000 read <$> lookupEnv "PORT"
          log' "Running any pending migrations..."
          (ctxt, app') <- app
          fs <- filter (".sql" `isSuffixOf`) <$> listDirectory "migrations"
          ms <- mapM (\f -> do c <- T.readFile ("migrations/" <> f)
                               T.putStrLn (T.reverse $ T.drop 4 $ T.reverse $ T.pack f)
                               return (T.reverse $ T.drop 4 $ T.reverse $ T.pack f, sql c "")) fs
          withResource (db ctxt) (\c -> do adaptor <- setupConn id c
                                           Rivet.main adaptor Rivet.MigrateUp ms)
          log' ("Starting server on port " <> pack (show port))
          catch (run port app')
                (\(_ :: SomeException) ->
                   log' "Shutting down...")
