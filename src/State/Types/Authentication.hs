module State.Types.Authentication where
import           Data.Text                            (Text)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
data Authentication =
     Authentication { id        :: Int
                    , accountId :: Int
                    , emailId   :: Int
                    , createdAt :: UTCTime
                    , token     :: Text
                    } deriving (Eq, Show)

instance FromRow Authentication where
  fromRow = Authentication <$> field
                           <*> field
                           <*> field
                           <*> field
                           <*> field
