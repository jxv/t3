module T3.Storage.JsonImpl
  ( writePlayback
  ) where

import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import T3.Match (MatchId(..))
import T3.Storage hiding (Storage(..))

writePlayback :: FilePath -> Playback -> IO ()
writePlayback prefix pb = BL.writeFile path (encode pb)
  where
    (MatchId matchIdText) = _pbMatchId pb
    path = prefix `mappend` (T.unpack $ matchIdText  `mappend` ".json")
