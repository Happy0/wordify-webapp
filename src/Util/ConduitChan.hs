module Util.ConduitChan (chanSource) where

import ClassyPrelude (IO, Int, MonadIO (liftIO), TChan, undefined)
import qualified Conduit as C
import Control.Concurrent.STM (atomically, readTChan)

chanSource :: TChan a -> C.ConduitT () a IO ()
chanSource chan = do
  let loop = do
        msg <- liftIO (atomically (readTChan chan))
        C.yield msg
        loop
  loop