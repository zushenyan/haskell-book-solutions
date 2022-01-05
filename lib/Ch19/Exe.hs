module Ch19.Exe where

import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset = addUTCTime (offset * 24 * 3600) <$> getCurrentTime

textUuid :: IO T.Text
textUuid = T.pack . UUID.toString <$> UUIDv4.nextRandom