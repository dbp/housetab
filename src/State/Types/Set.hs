{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module State.Types.Set where

import           Data.Csv                             as CSV
import           Data.Text
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Time.Clock
import           Data.Time.Format
import           Database.PostgreSQL.Simple.FromField as PG
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField   as PG
import           Database.PostgreSQL.Simple.Types
import           GHC.Generics

data SetType = Export | Import deriving (Eq, Show)

instance PG.FromField SetType where
  fromField f dat =
    do r <- PG.fromField f dat
       case r :: Text of
         "import" -> return Import
         "export" -> return Export
         _        -> returnError Incompatible f ""

instance PG.ToField SetType where
  toField Export = PG.toField ("export" :: Text)
  toField Import = PG.toField ("import" :: Text)

data Set = Set { id         :: Int
               , typ        :: SetType
               , created_at :: UTCTime
               , count      :: Int
               , archived   :: Int
               } deriving (Eq, Show)

instance FromRow Set where
  fromRow = Set <$> field
                <*> field
                <*> field
                <*> field
                <*> field

data ExportEntry =
     ExportEntry { eid         :: Int
                 , who         :: Text
                 , description :: Text
                 , date        :: UTCTime
                 , howmuch     :: Double
                 , whopays     :: [Text]
                 } deriving (Eq, Show, Generic)


instance CSV.FromField UTCTime where
  parseField t = parseTimeM True defaultTimeLocale "%F" (T.unpack . T.decodeUtf8 $ t)

instance CSV.ToField UTCTime where
  toField t = T.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%F" t

instance CSV.FromField [Text] where
  parseField t = pure $ T.splitOn "|" $ T.decodeUtf8 t

instance CSV.ToField [Text] where
  toField ts = T.encodeUtf8 $ T.intercalate "|" ts

instance FromNamedRecord ExportEntry where
    parseNamedRecord m =
      ExportEntry <$> m .: "id"
                  <*> m .: "who"
                  <*> m .: "description"
                  <*> m .: "date"
                  <*> m .: "howmuch"
                  <*> m .: "whopays"
instance ToNamedRecord ExportEntry where
    toNamedRecord (ExportEntry id who desc date howmuch whopays) = namedRecord [
          "id" .= id
        , "who" .= who
        , "description" .= desc
        , "date" .= date
        , "howmuch" .= howmuch
        , "whopays" .= whopays]
instance DefaultOrdered ExportEntry where
    headerOrder _ = header ["id"
                           , "who"
                           , "description"
                           , "date"
                           , "howmuch"
                           , "whopays"]
