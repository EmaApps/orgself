{-# LANGUAGE TypeApplications #-}

module Memoir.Data where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar
import Ema
import Memoir.Data.Measure (Measures, parseMeasures)
import System.FSNotify
  ( Event (..),
    watchDir,
    withManager,
  )
import System.FilePath (takeFileName, (</>))
import System.FilePattern.Directory (getDirectoryFiles)

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
  modelRoutes diary =
    Index : fmap OnDay (Map.keys $ diaryCal diary)

data Diary = Diary
  { diaryCal :: Map Day (OrgFile, Measures),
    diaryTags :: Map Text (Set Day)
  }
  deriving (Eq)

emptyDiary :: Diary
emptyDiary = Diary mempty mempty

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
          diaryTags = foldl' (addTag day) (diaryTags diary) (extractTags orgFile)
        }
    DiaryDel day ->
      diary
        { diaryCal =
            Map.delete day $ diaryCal diary,
          diaryTags =
            maybe
              (diaryTags diary)
              (foldl' (delTag day) (diaryTags diary) . extractTags . fst)
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

parseDailyNote :: FilePath -> IO (Maybe (Day, OrgFile))
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

diaryFrom :: FilePath -> IO Diary
diaryFrom folder = do
  putStrLn $ "Loading .org files from " <> folder
  fs <- getDirectoryFiles folder (one "*.org")
  updates <- fmap (uncurry DiaryAdd) . catMaybes <$> forM fs (parseDailyNote . (folder </>))
  let diary = foldl' (flip diaryUpdate) emptyDiary updates
  pure diary

watchAndUpdateDiary :: FilePath -> LVar.LVar Diary -> IO ()
watchAndUpdateDiary folder model = do
  putStrLn $ "Watching .org files in " <> folder
  withManager $ \mgr -> do
    stop <- watchDir mgr folder (const True) $ \event -> do
      print event
      let updateFile fp = do
            parseDailyNote fp >>= \case
              Nothing -> pure ()
              Just (day, org) -> do
                putStrLn $ "Update: " <> show day
                LVar.modify model $ diaryUpdate $ DiaryAdd day org
          deleteFile fp = do
            whenJust (parseDailyNoteFilepath fp) $ \day -> do
              putStrLn $ "Delete: " <> show day
              LVar.modify model $ diaryUpdate $ DiaryDel day
      case event of
        Added fp _ isDir -> unless isDir $ updateFile fp
        Modified fp _ isDir -> unless isDir $ updateFile fp
        Removed fp _ isDir -> unless isDir $ deleteFile fp
        Unknown fp _ _ -> updateFile fp
    threadDelay maxBound
      `finally` stop

extractTags :: OrgFile -> Set Text
extractTags (Org.OrgFile _meta (Org.OrgDoc blocks sections)) =
  foldMap fromBlocks blocks <> foldMap fromSections sections
  where
    fromSections :: Org.Section -> Set Text
    fromSections (Org.Section heading tags (Org.OrgDoc bs ss)) =
      Set.fromList tags
        <> foldMap fromWords heading
        <> foldMap fromBlocks bs
        <> foldMap fromSections ss

    fromBlocks :: Org.Block -> Set Text
    fromBlocks = \case
      Org.Paragraph ws ->
        foldMap fromWords ws
      _ ->
        mempty

    fromWords :: Org.Words -> Set Text
    fromWords = \case
      Org.Tags ts -> Set.fromList (toList ts)
      _ -> mempty
