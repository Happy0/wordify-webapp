module Util.Exception (printAndIgnoreSyncException) where

import ClassyPrelude (Bool, Either (Left, Right), IO, Maybe, SomeException, pure, putStrLn, show, unless, ($), (>>))
import Control.Error (isNothing)
import Control.Exception (AsyncException, fromException, throw)
import Data.Semigroup ((<>))
import Data.Text (pack)

printAndIgnoreSyncException :: Either SomeException a -> IO ()
printAndIgnoreSyncException e = do
  case e of
    Left ex -> do
      putStrLn ("Exception: " <> pack (show ex))
      unless (isSyncException ex) $ putStrLn (pack (show ex)) >> throw ex
    Right _ -> pure ()

isSyncException :: SomeException -> Bool
isSyncException e = isNothing (fromException e :: Maybe AsyncException)