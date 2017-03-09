module State.Types.Account where
import           Data.Text                            (Text)


import           Database.PostgreSQL.Simple.FromRow

data Account =
     Account { id   :: Int
             , name :: Text
             } deriving (Eq, Show)

instance FromRow Account where
  fromRow = Account <$> field
                    <*> field
