module Model.Api (ServerMessage, commandName, toJSONResponse) where

    import Data.ByteString.Lazy
    import Data.Text
    import Data.Aeson

    class (ToJSON a) => ServerMessage a where
        commandName :: a -> Text

        toJSONResponse :: a -> ByteString
        toJSONResponse command = encode (object ["command" .= commandName command, "payload" .= toJSON command])