module Model.Api (ServerMessage, ClientError(ClientError), Locale, GameID, commandName, toJSONMessage, toJSONResponse) where

    import Data.Aeson
    import Data.ByteString.Lazy
    import Data.Maybe
    import Data.Text
    import Wordify.Rules.Board

    type Locale = Text

    type GameID = Text

    {-
        Represents an error on bad input from the client such as sending
        invalid JSON or a malformed command.
    -}
    newtype ClientError = ClientError {reason :: Text}

    instance ToJSON ClientError where
        toJSON serverError = object ["reason" .= reason serverError]

    instance ServerMessage ClientError where
        commandName _ = "error"

    {-
        A server message contains a description of the command to the client
        and a payload for the command that should be carried out. The command
        payload must be serializable to JSON.
    -}
    class (ToJSON a) => ServerMessage a where
        commandName :: a -> Text

        toJSONMessage :: a -> Value
        toJSONMessage command = object ["command" .= commandName command, "payload" .= toJSON command]

        toJSONResponse :: a -> ByteString
        toJSONResponse command = encode (toJSONMessage command)
