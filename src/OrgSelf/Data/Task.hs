{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module OrgSelf.Data.Task where

import qualified Data.Org as Org

data Task = Task
  { taskState :: Org.Todo,
    taskHeading :: NonEmpty Org.Words
  }
  deriving (Eq, Show)

parseTask :: Org.Section -> Maybe Task
parseTask Org.Section {..} = do
  taskState <- sectionTodo
  let taskHeading = sectionHeading
  pure Task {..}

queryTasks :: Org.OrgFile -> [Task]
queryTasks (Org.OrgFile _ doc) = do
  fromDoc doc
  where
    fromDoc :: Org.OrgDoc -> [Task]
    fromDoc (Org.OrgDoc _blocks sections) =
      catMaybes (parseTask <$> sections)
        <> foldMap fromDoc (Org.sectionDoc <$> sections)
