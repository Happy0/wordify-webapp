module Handler.Common.Chat
  ( ChatMessage (ChatMessage),
    sendChatUpdate,
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Network.WebSockets.Connection as C
import Prelude (($), IO, Int)

data ChatMessage = ChatMessage {user :: Text, message :: Text, when :: UTCTime, messageNumber :: Int}

instance ToJSON ChatMessage where
  toJSON (ChatMessage user message when messageNumber) = object ["player" .= user, "message" .= message, "when" .= when, "messageNumber" .= messageNumber]

sendChatUpdate :: C.Connection -> ChatMessage -> IO ()
sendChatUpdate connection chatMessage =
  C.sendTextData connection $ encode $ object
    [ "command" .= ("playerChat" :: Text)
    , "payload" .= toJSON chatMessage
    ]
