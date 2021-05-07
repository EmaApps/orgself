{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module OrgSelf.Data where

import Control.Exception (throwIO)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Calendar (Day)
import OrgSelf.Data.Measure (Measures, parseMeasures)
import OrgSelf.Data.Tag (TagStore)
import qualified OrgSelf.Data.Tag as Tag
import OrgSelf.Data.Task (Task)
import qualified OrgSelf.Data.Task as Task
import System.FilePath (takeFileName)
import qualified Text.Megaparsec as M

data Diary = Diary
  { diaryCal :: Map Day (OrgFile, Measures),
    diaryTasks :: Map Day [Task],
    diaryTags :: TagStore
  }
  deriving (Eq)

instance Default Diary where
  def = Diary mempty mempty mempty

diaryLookup :: Day -> Diary -> Maybe (OrgFile, Measures)
diaryLookup x =
  Map.lookup x . diaryCal

diaryLookupTasks :: Day -> Diary -> [Task]
diaryLookupTasks x =
  fromMaybe mempty . Map.lookup x . diaryTasks

data DiaryUpdate
  = DiaryModify Day OrgFile
  | DiaryDel Day
  deriving (Eq, Show)

-- TODO: Use lens to make this nicer.
diaryUpdate :: DiaryUpdate -> Diary -> Diary
diaryUpdate action diary =
  case action of
    DiaryModify day orgFile ->
      diary
        { diaryCal =
            diaryCal diary
              & Map.insert
                day
                (orgFile, parseMeasures $ Org.orgMeta orgFile),
          diaryTasks =
            diaryTasks diary
              & Map.insert
                day
                (Task.queryTasks orgFile),
          diaryTags =
            diaryTags diary
              & Tag.delTagsForDay day
              & Tag.addTagsForDay day (Tag.tagsFrom orgFile)
        }
    DiaryDel day ->
      diary
        { diaryCal =
            diaryCal diary
              & Map.delete day,
          diaryTasks =
            diaryTasks diary
              & Map.delete day,
          diaryTags =
            diaryTags diary
              & Tag.delTagsForDay day
        }

parseDay :: String -> Maybe Day
parseDay =
  parseTimeM False defaultTimeLocale "%Y-%m-%d"

-- | Return Nothing if `f` is not a daily note filepath.
parseDailyNote :: MonadIO m => FilePath -> m (Maybe (Day, OrgFile))
parseDailyNote f =
  case parseDailyNoteFilepath f of
    Nothing -> pure Nothing
    Just day -> do
      s <- readFileText f
      -- orgFile <- liftIO $ parseMust Org.orgFile f s
      pure $ (day,) <$> Org.org s

parseDailyNoteFilepath :: FilePath -> Maybe Day
parseDailyNoteFilepath f =
  parseDay . toString =<< T.stripSuffix ".org" (toText $ takeFileName f)

-- FIXME: Not using this until https://github.com/srid/ema/issues/21
parseMust :: M.Parsec Void Text a -> FilePath -> Text -> IO a
parseMust p fn s =
  case parse p fn s of
    Left e -> do
      liftIO $ putStrLn "fuck"
      throwIO $ OrgParseError e
    Right v -> pure v
  where
    parse :: M.Parsec Void Text a -> FilePath -> Text -> Either Text a
    parse p' fn' s' =
      first (toText . M.errorBundlePretty) $
        M.parse (p' <* M.eof) fn' s'

newtype OrgParseError = OrgParseError Text
  deriving (Eq, Show, Exception)
