{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module OrgSelf.Data.Tag where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tagged
import Data.Time.Calendar (Day)

type Tag = Tagged "Tag" Text

type TagStore = Map Tag (Set Day)

addTagForDay :: Day -> Tag -> TagStore -> TagStore
addTagForDay v k m =
  let vs = fromMaybe mempty $ Map.lookup k m
   in Map.insert k (Set.insert v vs) m

delTagFromDay :: Day -> Tag -> TagStore -> TagStore
delTagFromDay v k m =
  case Map.lookup k m of
    Nothing -> m
    Just vs ->
      if Set.member v vs
        then Map.insert k (Set.delete v vs) m
        else m
