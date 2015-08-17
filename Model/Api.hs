module Model.Api (ServerMessage, commandName, toJSONResponse) where

    import Data.ByteString.Lazy
    import Data.Text
    import Data.Aeson

    {-
        A server message contains a description of the command to the client
        and a payload for the command that should be carried out. The command
        payload must be serializable to JSON. 
    -}
    class (ToJSON a) => ServerMessage a where
        commandName :: a -> Text

        toJSONResponse :: a -> ByteString
        toJSONResponse command = encode (object ["command" .= commandName command, "payload" .= toJSON command])