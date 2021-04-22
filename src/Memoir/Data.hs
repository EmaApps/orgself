{-# LANGUAGE TypeApplications #-}

module Memoir.Data where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Memoir.Data.Measure (Measures, parseMeasures)
import System.FSNotify
  ( Event (..),
    watchDir,
    withManager,
  )
import System.FilePath (takeFileName, (</>))
import System.FilePattern.Directory (getDirectoryFiles)

data Diary = Diary
  { diaryCal :: Map Day (OrgFile, Measures),
    diaryTags :: Map Text (NonEmpty Day)
  }
  deriving (Eq)

emptyDiary :: Diary
emptyDiary = Diary mempty mempty

data DiaryUpdate
  = DiaryAdd Day OrgFile
  | DiaryDel Day
  deriving (Eq)

-- TODO: Update @diaryTags@ !
diaryUpdate :: DiaryUpdate -> Diary -> Diary
diaryUpdate action diary =
  case action of
    DiaryAdd day orgFile ->
      diary
        { diaryCal =
            Map.insert day (orgFile, parseMeasures $ Org.orgMeta orgFile) $ diaryCal diary
        }
    DiaryDel day ->
      diary
        { diaryCal =
            Map.delete day $ diaryCal diary
        }

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
