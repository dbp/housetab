module State.Types.Email where
import           Data.Text                            (Text)
import           Data.Time.Clock

import           Database.PostgreSQL.Simple.FromRow

data Email =
     Email { id         :: Int
           , accountId  :: Int
           , email      :: Text
           , verifiedAt :: Maybe UTCTime
           , token      :: Text
           } deriving (Eq, Show)

instance FromRow Email where
  fromRow = Email <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
