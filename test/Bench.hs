{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Criterion.Types
import           Data.Text                  (Text)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Control.Monad.State

import           Web.Larceny

import qualified Handler.Home

main :: IO ()
main = do
  tpl <- parse <$> TIO.readFile "templates/entry/list.tpl"
  es <- read . T.unpack <$> TIO.readFile "test/entries.txt"
  ps <- read . T.unpack <$> TIO.readFile "test/people.txt"
  defaultMainWith (defaultConfig {reportFile = Just "report.html"}) [
      bench "entries" $ nfIO $
        evalStateT
          (runTemplate tpl
                       []
                       (subs [("entries",
                               mapSubs (Handler.Home.entrySubs ps) es)])
                       M.empty)
          () 
    ]

