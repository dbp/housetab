module State.Types.Share where

import           Data.Time.Clock

import           Database.PostgreSQL.Simple.FromRow

data Share =
     Share { id       :: Int
           , personId :: Int
           , start    :: UTCTime
           , value    :: Double
           } deriving (Eq, Show, Ord)

instance FromRow Share where
  fromRow = Share <$> field
                  <*> field
                  <*> field
                  <*> field
