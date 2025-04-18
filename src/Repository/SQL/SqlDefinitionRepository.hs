module Repository.SQL.SqlDefinitionRepository (DefinitionRepositorySQLBackend (DefinitionRepositorySQLBackend), getDefinitionsImpl) where

import ClassyPrelude (IO, Int, Maybe (Just, Nothing), Ord, Ordering, UTCTime, compare, flip, fromMaybe, fst, liftIO, map, mapM, pure, return, sortBy, uncurry, undefined, zipWith, ($), (++), (.))
import Conduit (ConduitT)
import Data.Conduit.List (sourceList)
import qualified Data.Map.Strict as Map
import Data.Pool
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Database.Persist.Sql
import qualified Model as M
import Repository.DefinitionRepository (DefinitionRepository (getDefinitions, getGameDefinitions, saveGameDefinitions), GameWordItem (GameWordItem), WordDefinitionItem (WordDefinitionItem))

data DefinitionRepositorySQLBackend = DefinitionRepositorySQLBackend (Pool SqlBackend)

instance DefinitionRepository DefinitionRepositorySQLBackend where
  getDefinitions (DefinitionRepositorySQLBackend pool) word = getDefinitionsImpl pool word
  saveGameDefinitions (DefinitionRepositorySQLBackend pool) when gameId word definitions = saveGameDefinitionsImpl pool when gameId word definitions
  getGameDefinitions (DefinitionRepositorySQLBackend pool) gameId = getGameDefinitionsImpl pool gameId

getDefinitionsImpl :: Pool SqlBackend -> T.Text -> IO [WordDefinitionItem]
getDefinitionsImpl pool = undefined

getGameDefinitionsImpl :: Pool SqlBackend -> T.Text -> ConduitT () GameWordItem IO ()
getGameDefinitionsImpl pool gameId = do
  defs <- liftIO $ withPool pool $ do
    E.select $ do
      E.from $ \(gameDef `E.LeftOuterJoin` def) -> do
        E.on (gameDef E.^. M.GameDefinitionDefinitionId E.==. def E.?. M.DefinitionId)
        E.where_ (gameDef E.^. M.GameDefinitionGameId E.==. E.val (M.GameKey gameId))
        return (gameDef, def)
  -- TODO: stream from database using appropriate query rather than loading all into memory and grouping
  sourceList (makeGameWordDefinitions defs)

makeGameWordDefinitions :: [(E.Entity M.GameDefinition, Maybe (E.Entity M.Definition))] -> [GameWordItem]
makeGameWordDefinitions definitions =
  let entities = map extractValues definitions
   in let groupedByWordAndTimestamp = groupByKey (extractWordAndTimeStamp . fst) entities
       in makeGameWorkItems groupedByWordAndTimestamp
  where
    makeGameWorkItems :: Map.Map (T.Text, UTCTime) [(M.GameDefinition, Maybe M.Definition)] -> [GameWordItem]
    makeGameWorkItems grouped = map (uncurry makeGameWordItem) (Map.toList grouped)

    makeGameWordItem :: (T.Text, UTCTime) -> [(M.GameDefinition, Maybe M.Definition)] -> GameWordItem
    makeGameWordItem (word, createdAt) defs =
      let wordsWithDefinitions = fromMaybe [] (mapM definitionOrEmpty defs)
       in let wordDefinitions = map makeDefinition wordsWithDefinitions
           in GameWordItem word createdAt wordDefinitions
      where
        definitionOrEmpty :: (M.GameDefinition, Maybe M.Definition) -> Maybe (M.GameDefinition, M.Definition)
        definitionOrEmpty (gameDef, Just def) = Just (gameDef, def)
        definitionOrEmpty (_, Nothing) = Nothing

        sortDefinitions :: [(M.GameDefinition, M.Definition)] -> [(M.GameDefinition, M.Definition)]
        sortDefinitions x = sortBy comparator x

        comparator :: (M.GameDefinition, M.Definition) -> (M.GameDefinition, M.Definition) -> Ordering
        comparator (gameDef1, def1) (gameDef2, def2) = compare (definitionNumber gameDef1) (definitionNumber gameDef2)

        definitionNumber :: M.GameDefinition -> Int
        definitionNumber (M.GameDefinition _ defNumber _ _ _) = defNumber

    makeDefinition :: (M.GameDefinition, M.Definition) -> WordDefinitionItem
    makeDefinition (gameDef, def) =
      let (M.Definition _ partOfSpeech definition example) = def
       in WordDefinitionItem partOfSpeech definition example

    extractValues :: (E.Entity M.GameDefinition, Maybe (E.Entity M.Definition)) -> (M.GameDefinition, Maybe M.Definition)
    extractValues (Entity _ gameDef, Just (Entity _ def)) = (gameDef, Just def)
    extractValues (Entity _ gameDef, Nothing) = (gameDef, Nothing)

    extractWordAndTimeStamp :: M.GameDefinition -> (T.Text, UTCTime)
    extractWordAndTimeStamp (M.GameDefinition word _ createdAt _ _) = (word, createdAt)

    groupByKey :: (Ord k) => (a -> k) -> [a] -> Map.Map k [a]
    groupByKey toKey xs = Map.fromListWith (++) [(toKey x, [x]) | x <- xs]

saveGameDefinitionsImpl :: Pool SqlBackend -> UTCTime -> T.Text -> T.Text -> [WordDefinitionItem] -> IO ()
saveGameDefinitionsImpl pool when gameId word [] = withPool pool $ do
  let gameDefinition = M.GameDefinition word 1 when Nothing (M.GameKey gameId)
  insert_ gameDefinition
saveGameDefinitionsImpl pool when gameId word definitions = do
  -- TODO: do this in SQL with persist bulk upsert / raw SQL if necessary
  defs <- mapM (upsertDefinition pool when word) definitions
  withPool pool $ do
    let gameDefinitions = zipWith (makeGameDefinition when gameId word) [1 ..] defs
    insertMany_ gameDefinitions

makeGameDefinition :: UTCTime -> T.Text -> T.Text -> Int -> Key M.Definition -> M.GameDefinition
makeGameDefinition createdAt gameId word definitionNumber definitionId =
  M.GameDefinition word definitionNumber createdAt (Just definitionId) (M.GameKey gameId)

upsertDefinition :: Pool SqlBackend -> UTCTime -> T.Text -> WordDefinitionItem -> IO (Key M.Definition)
upsertDefinition pool when word definition@(WordDefinitionItem partOfSpeech wordDefinition _) = withPool pool $ do
  let dbModel = gameDefinitionDatabaseModel word definition
  (Entity entityKey entity) <- upsertBy (M.UniqueWordPartOfSpeechDefinition word partOfSpeech wordDefinition) dbModel []
  pure entityKey

gameDefinitionDatabaseModel :: T.Text -> WordDefinitionItem -> M.Definition
gameDefinitionDatabaseModel word (WordDefinitionItem partOfSpeech definition example) =
  M.Definition word partOfSpeech definition example

withPool pool = flip runSqlPersistMPool pool