module State.Types.Entry where
import           Data.Text                            (Text)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types

data Entry =
     Entry { id          :: Int
           , accountId   :: Int
           , whoId       :: Int
           , description :: Text
           , date        :: UTCTime
           , howmuch     :: Double
           , whopaysIds  :: [Int]
           } deriving (Eq, Show)

instance FromRow Entry where
  fromRow = Entry <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> (fromPGArray <$> field)
