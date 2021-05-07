{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module OrgSelf.Data.Tag where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tagged
import Data.Time.Calendar (Day)

type Tag = Tagged "Tag" Text

type TagStore = Map Tag (Set Day)

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

delTagFromDay :: Day -> Tag -> TagStore -> TagStore
delTagFromDay v k m =
  case Map.lookup k m of
    Nothing -> m
    Just vs ->
      if Set.member v vs
        then Map.insert k (Set.delete v vs) m
        else m
