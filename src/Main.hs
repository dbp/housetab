{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Configuration.Dotenv     (loadFile)
import           Control.Exception        (SomeException (..), catch)
import           Control.Logging          (log', withStdoutLogging)
import           Control.Monad            (when)
import           Data.Monoid              ((<>))
import           Data.Text                (pack)
import           Network.Wai              (Middleware, pathInfo)
import           Network.Wai.Handler.Warp (run)
import           Site                     (app)
import           System.Directory         (doesFileExist)
import           System.Environment       (lookupEnv)

main :: IO ()
main = withStdoutLogging $
       do envExists <- doesFileExist ".env"
          when envExists $ loadFile False ".env"
          port <- maybe 8000 read <$> lookupEnv "PORT"
          log' ("Starting server on port " <> pack (show port))
          app' <- app
          catch (run port app')
                (\(_ :: SomeException) ->
                   log' "Shutting down...")
