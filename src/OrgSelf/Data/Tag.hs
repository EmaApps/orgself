{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module OrgSelf.Data.Tag where

import qualified Data.Map.Strict as Map
import qualified Data.Org as Org
import qualified Data.Set as Set
import Data.Tagged
import Data.Time.Calendar (Day)

type Tag = Tagged "Tag" Text

type TagStore = Map Tag (Set Day)

tagsFrom :: Org.OrgFile -> Set Tag
tagsFrom =
  Set.map Tagged . Org.allDocTags . Org.orgDoc

addTagsForDay :: Day -> Set Tag -> TagStore -> TagStore
addTagsForDay day =
  flip $ foldl' (flip (addTagForDay day))
  where
    addTagForDay :: Day -> Tag -> TagStore -> TagStore
    addTagForDay v k m =
      let vs = fromMaybe mempty $ Map.lookup k m
       in Map.insert k (Set.insert v vs) m

delTagsForDay :: Day -> TagStore -> TagStore
delTagsForDay day =
  Map.map $ Set.delete day
