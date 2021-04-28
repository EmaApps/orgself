{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module OrgSelf.Data.Task where

import qualified Data.Org as Org

data Task = Task
  { taskState :: TaskState,
    taskHeading :: NonEmpty Org.Words
  }
  deriving (Eq, Show)

data TaskState = TODO | DONE
  deriving (Eq, Show, Ord, Bounded, Enum)

parseTask :: Org.Section -> Maybe Task
parseTask Org.Section {..} = do
  firstWord <- case head sectionHeading of
    Org.Plain s -> pure s
    _ -> Nothing
  taskHeading <- nonEmpty $ tail sectionHeading
  taskState <-
    listToMaybe . catMaybes $
      [minBound .. maxBound] <&> \st -> do
        guard $ show st == firstWord
        pure st
  pure Task {..}

queryTasks :: Org.OrgFile -> [Task]
queryTasks (Org.OrgFile _ doc) = do
  fromDoc doc
  where
    fromDoc :: Org.OrgDoc -> [Task]
    fromDoc (Org.OrgDoc _blocks sections) =
      catMaybes (parseTask <$> sections)
        <> foldMap fromDoc (Org.sectionDoc <$> sections)
