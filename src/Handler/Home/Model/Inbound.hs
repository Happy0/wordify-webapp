module Handler.Home.Model.Inbound where

import ClassyPrelude
import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), withObject, (.:))

data InboundHomeMessage
  = SendChatMessage Text

instance FromJSON InboundHomeMessage where
  parseJSON = withObject "InboundHomeMessage" $ \v -> do
    cmd <- v .: "command"
    case (cmd :: Text) of
      "say" -> do
        payload <- v .: "payload"
        msg     <- payload .: "message"
        pure (SendChatMessage msg)
      other -> fail $ "Unknown command: " <> unpack other
