{-# LANGUAGE TypeApplications #-}

module OrgSelf.Route where

import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (Tagged), untag)
import Data.Time.Calendar
import Ema (Ema (..))
import qualified Ema
import OrgSelf.Data (Diary)
import qualified OrgSelf.Data as Data

data Route
  = Index
  | OnDay Day
  | Tag Data.Tag
  deriving (Show)

instance Ema Diary Route where
  encodeRoute = \case
    Index -> mempty
    OnDay day ->
      let (y, m, d) = toGregorian day
       in [show y, show m, show d]
    Tag tag ->
      ["tag", fromString . toString . untag $ tag]
  decodeRoute = \case
    [] -> Just Index
    ["tag", tag] ->
      Just $ Tag $ Tagged $ Ema.unSlug tag
    [y, m, d] ->
      OnDay <$> do
        y' <- readMaybe @Integer (toString $ Ema.unSlug y)
        m' <- readMaybe @Int (toString $ Ema.unSlug m)
        d' <- readMaybe @Int (toString $ Ema.unSlug d)
        fromGregorianValid y' m' d'
    _ -> Nothing
  staticRoutes diary =
    Index : fmap OnDay (Map.keys $ Data.diaryCal diary)
