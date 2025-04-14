module Repository.DefinitionRepository (
    toDefinitionRepositoryImpl,
    DefinitionRepository(getDefinitions, saveGameDefinitions, getGameDefinitions),
    DefinitionRepositoryImpl(DefinitionRepositoryImpl, getWordDefinitions, saveGameWordDefinitions, getGameWordDefinitions),
    GameWordItem(GameWordItem),
    WordDefinitionItem(WordDefinitionItem)) where

    import Conduit (ConduitT)
    import ClassyPrelude (IO, UTCTime, Maybe)
    import qualified Data.Text as T

    data WordDefinitionItem = WordDefinitionItem { partOfSpeech :: T.Text, definition :: T.Text, example :: Maybe T.Text }
    data GameWordItem = GameWordItem { word :: T.Text, when :: UTCTime, definitions :: [WordDefinitionItem] }

    class DefinitionRepository a where
        getDefinitions :: a -> T.Text -> IO [WordDefinitionItem]
        saveGameDefinitions :: a -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO ()
        getGameDefinitions :: a -> T.Text -> ConduitT () GameWordItem IO ()

    data DefinitionRepositoryImpl = DefinitionRepositoryImpl {
        getWordDefinitions :: T.Text -> IO [WordDefinitionItem],
        saveGameWordDefinitions :: UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO (),
        getGameWordDefinitions :: T.Text -> ConduitT () GameWordItem IO ()
    }

    toDefinitionRepositoryImpl :: DefinitionRepository a => a -> DefinitionRepositoryImpl
    toDefinitionRepositoryImpl repository = DefinitionRepositoryImpl (getDefinitions repository) (saveGameDefinitions repository) (getGameDefinitions repository)