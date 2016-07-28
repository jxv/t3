module T3.Server.StorageImpl
  ( writePlayback
  ) where

import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import T3.Server (MatchId(..))
import T3.Server.Storage hiding (Storage(..))

writePlayback :: FilePath -> Playback -> IO ()
writePlayback prefix pb = BL.writeFile path (encode pb)
  where
    (MatchId matchIdText) = _pbMatchId pb
    path = prefix `mappend` (T.unpack $ matchIdText  `mappend` ".json")
