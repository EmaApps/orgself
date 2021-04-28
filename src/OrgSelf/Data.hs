{-# LANGUAGE TypeApplications #-}

module OrgSelf.Data where

import Control.Monad.Logger
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar
import Ema
import qualified Ema.Helper.FileSystem as FileSystem
import OrgSelf.Data.Measure (Measures, parseMeasures)
import OrgSelf.Data.Task (Task)
import qualified OrgSelf.Data.Task as Task
import System.FilePath (takeFileName, (</>))
import UnliftIO (MonadUnliftIO)

data Route
  = Index
  | OnDay Day
  | Tag Text
  deriving (Show)

instance Ema Diary Route where
  encodeRoute = \case
    Index -> mempty
    OnDay day ->
      let (y, m, d) = toGregorian day
       in [show y, show m, show d]
    Tag tag ->
      ["tag", fromString . toString $ tag]
  decodeRoute = \case
    [] -> Just Index
    ["tag", tag] ->
      Just $ Tag $ unSlug tag
    [y, m, d] ->
      OnDay <$> do
        y' <- readMaybe @Integer (toString $ unSlug y)
        m' <- readMaybe @Int (toString $ unSlug m)
        d' <- readMaybe @Int (toString $ unSlug d)
        fromGregorianValid y' m' d'
    _ -> Nothing
  staticRoutes diary =
    Index : fmap OnDay (Map.keys $ diaryCal diary)

data Diary = Diary
  { diaryCal :: Map Day (OrgFile, Measures),
    diaryTasks :: Map Day [Task],
    diaryTags :: Map Text (Set Day)
  }
  deriving (Eq)

emptyDiary :: Diary
emptyDiary = Diary mempty mempty mempty

diaryLookup :: Day -> Diary -> Maybe (OrgFile, Measures)
diaryLookup x =
  Map.lookup x . diaryCal

diaryLookupTasks :: Day -> Diary -> [Task]
diaryLookupTasks x =
  fromMaybe mempty . Map.lookup x . diaryTasks

data DiaryUpdate
  = DiaryAdd Day OrgFile
  | DiaryDel Day
  deriving (Eq)

diaryUpdate :: DiaryUpdate -> Diary -> Diary
diaryUpdate action diary =
  case action of
    DiaryAdd day orgFile ->
      diary
        { diaryCal =
            Map.insert day (orgFile, parseMeasures $ Org.orgMeta orgFile) $ diaryCal diary,
          diaryTasks =
            Map.insert day (Task.queryTasks orgFile) $ diaryTasks diary,
          diaryTags =
            foldl' (addTag day) (diaryTags diary) (Org.allDocTags . Org.orgDoc $ orgFile)
        }
    DiaryDel day ->
      diary
        { diaryCal =
            Map.delete day $ diaryCal diary,
          diaryTasks =
            Map.delete day $ diaryTasks diary,
          diaryTags =
            maybe
              (diaryTags diary)
              (foldl' (delTag day) (diaryTags diary) . Org.allDocTags . Org.orgDoc . fst)
              (Map.lookup day $ diaryCal diary)
        }
  where
    addTag :: Day -> Map Text (Set Day) -> Text -> Map Text (Set Day)
    addTag v m k =
      let vs = fromMaybe mempty $ Map.lookup k m
       in Map.insert k (Set.insert v vs) m
    delTag :: Day -> Map Text (Set Day) -> Text -> Map Text (Set Day)
    delTag v m k =
      case Map.lookup k m of
        Nothing -> m
        Just vs ->
          if Set.member v vs
            then Map.insert k (Set.delete v vs) m
            else m

parseDay :: String -> Maybe Day
parseDay =
  parseTimeM False defaultTimeLocale "%Y-%m-%d"

parseDailyNote :: MonadIO m => FilePath -> m (Maybe (Day, OrgFile))
parseDailyNote f =
  case parseDailyNoteFilepath f of
    Nothing -> pure Nothing
    Just day -> do
      s <- readFileText f
      case Org.org s of
        Nothing -> pure Nothing
        Just orgFile -> pure $ Just (day, orgFile)

parseDailyNoteFilepath :: FilePath -> Maybe Day
parseDailyNoteFilepath f =
  parseDay . toString =<< T.stripSuffix ".org" (toText $ takeFileName f)

diaryFrom :: (MonadIO m, MonadLogger m) => FilePath -> m Diary
diaryFrom folder = do
  logInfoN $ "Loading .org files from " <> toText folder
  fs <- FileSystem.filesMatching folder ["*.org"]
  updates <- fmap (uncurry DiaryAdd) . catMaybes <$> forM fs (parseDailyNote . (folder </>))
  let diary = foldl' (flip diaryUpdate) emptyDiary updates
  pure diary

watchAndUpdateDiary :: (MonadIO m, MonadLogger m, MonadUnliftIO m) => FilePath -> LVar.LVar Diary -> m ()
watchAndUpdateDiary folder model = do
  logInfoN $ "Watching .org files in " <> toText folder
  FileSystem.onChange folder $ \fp -> \case
    FileSystem.Update ->
      parseDailyNote fp >>= \case
        Nothing -> pure ()
        Just (day, org) -> do
          putStrLn $ "Update: " <> show day
          LVar.modify model $ diaryUpdate $ DiaryAdd day org
    FileSystem.Delete ->
      whenJust (parseDailyNoteFilepath fp) $ \day -> do
        putStrLn $ "Delete: " <> show day
        LVar.modify model $ diaryUpdate $ DiaryDel day
