{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Set as Set
import Data.Some
import Data.Time.Calendar (addDays)
import Ema.App (runEma)
import qualified Ema.Helper.Tailwind as Tailwind
import Ema.Route (IsRoute (routeUrl))
import Memoir.Data (Diary)
import qualified Memoir.Data as Data
import Memoir.Data.Measure
import Memoir.Route (Route (..))
import qualified Memoir.Widget.Icons as Icons
import qualified Shower
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = do
  folder <-
    getArgs >>= \case
      [_, path] -> canonicalizePath path
      _ -> canonicalizePath "example"
  mainWith folder

mainWith :: FilePath -> IO ()
mainWith folder = do
  flip runEma render $ \model -> do
    LVar.set model =<< Data.diaryFrom folder
    Data.watchAndUpdateDiary folder model

render :: Diary -> Route -> LByteString
render diary r =
  Tailwind.layout (H.title "My Diary") $
    H.div ! A.class_ "container mx-auto" $ do
      let heading =
            H.header
              ! A.class_ "text-4xl my-2 py-2 font-bold text-center bg-purple-600 text-gray-100 shadow"
      case r of
        Index -> do
          heading "My Diary"
          H.div ! A.class_ "flex flex-col" $ do
            let usedMeasures = Set.fromList $ concat $ Map.elems diary <&> \(_, measures) -> DMap.keys measures
                td cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)
                th cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)
            -- Render diary index along with self-tracking measures, as a table.
            H.table ! A.class_ "table-auto" $ do
              H.tr ! A.class_ "bg-gray-50" $ do
                th "" ""
                forM_ usedMeasures $ \m -> do
                  th "font-bold" $ H.toHtml $ measureName m
              forM_ (sortOn (Down . fst) $ Map.toList diary) $ \(day, (_, dayMeasures)) -> do
                H.tr $ do
                  th "font-bold" $ routeDay day
                  forM_ usedMeasures $ \m -> do
                    let isCurr = \case
                          Measure_Extent s :=> Identity _ -> m == Some (Measure_Extent s)
                          Measure_Rating5 s :=> Identity _ -> m == Some (Measure_Rating5 s)
                    case find isCurr (DMap.assocs dayMeasures) of
                      Nothing -> td "" ""
                      Just measure -> td "" $ renderMeasureValue measure
        OnDay day -> do
          heading $ show day
          routeElem Index "Back to Index"
          H.div ! A.class_ "text-center text-3xl " $ do
            let yesterday = addDays (-1) day
                tomorrow = addDays 1 day
                renderOtherDay oDay =
                  if Map.member oDay diary
                    then routeDay oDay
                    else H.span ! A.class_ "text-gray-200" $ H.toMarkup @Text (show oDay)
            renderOtherDay yesterday
            " | "
            H.span ! A.class_ "font-bold" $ H.toMarkup @Text (show day)
            " | "
            renderOtherDay tomorrow
          maybe "not found" renderDay (Map.lookup day diary)
      H.footer
        ! A.class_ "text-xs my-4 py-2 text-center bg-gray-200"
        $ do
          "Powered by "
          H.a
            ! A.href "https://github.com/srid/memoir"
            ! A.target "blank_"
            ! A.class_ "text-purple font-bold"
            $ "memoir"
  where
    routeDay day =
      routeElem (OnDay day) $ H.toMarkup @Text (show day)
    routeElem r' w =
      H.a ! A.class_ "text-purple-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ routeUrl r')

renderDay :: (OrgFile, Measures) -> H.Html
renderDay (orgFile, measures) = do
  renderMeasures measures
  renderOrg orgFile

-- TODO: Move to separate module (after decoupling tailwind styling)
-- Even make it a separate library, as long as CSS classes can be customized!
renderOrg :: OrgFile -> H.Html
renderOrg _org@(Org.OrgFile _meta doc) = do
  let heading = H.header ! A.class_ "text-2xl my-2 font-bold"
  heading "Doc"
  -- renderAST "AST" org
  renderOrgDoc doc

renderMeasures :: Measures -> H.Html
renderMeasures meta = do
  H.table ! A.class_ "table-auto" $ do
    forM_ (DMap.toList meta) $ \measure ->
      H.tr $ renderMeasure measure

renderMeasure :: DSum Measure Identity -> H.Html
renderMeasure = \case
  x@(Measure_Rating5 name :=> Identity _) -> do
    td "font-bold" $ H.toMarkup name
    td "font-mono" $ renderMeasureValue x
  x@(Measure_Extent name :=> Identity _) -> do
    td "font-bold" $ H.toMarkup name
    td "font-mono" $ renderMeasureValue x
  where
    td cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)

renderMeasureValue :: DSum Measure Identity -> H.Html
renderMeasureValue = \case
  Measure_Rating5 _name :=> Identity rating -> do
    let (x, y) = ratingSplit rating
    replicateM_ x $ Icons.starSolid "inline h-6 w-6"
    replicateM_ y $ Icons.star "inline h-5 w-6"
  Measure_Extent _name :=> Identity extent -> do
    case extent of
      ExtentFull -> Icons.rss "inline h-6 w-6"
      ExtentSome -> Icons.rssHalf "inline h-6 w-6"
      ExtentNone -> "None"

renderOrgDoc :: Org.OrgDoc -> H.Html
renderOrgDoc (Org.OrgDoc blocks sections) = do
  whenNotNull blocks $ \_ -> do
    H.div ! A.class_ "" $
      forM_ blocks renderBlock
  renderOrgSections sections

renderOrgSections :: [Org.Section] -> H.Html
renderOrgSections sections =
  H.ul ! A.class_ "list-disc ml-8" $ do
    whenNotNull sections $ \_ -> do
      forM_ sections renderSection

renderBlock :: Org.Block -> H.Html
renderBlock blk = case blk of
  Org.Quote s -> H.blockquote (H.toHtml s)
  Org.Example _s -> renderAST "TODO: Example" blk
  Org.Code _mlang _s -> renderAST "TODO: Code" blk
  Org.List (Org.ListItems items) ->
    H.ul ! A.class_ "list-disc ml-4" $
      forM_ items $ \(Org.Item ws msub) ->
        H.li $ do
          renderWordsList ws
          whenJust msub $ \listItems ->
            renderBlock $ Org.List listItems
  Org.Table _rows -> renderAST "TODO: Table" blk
  Org.Paragraph ws -> H.p $ renderWordsList ws

renderSection :: Org.Section -> H.Html
renderSection (Org.Section heading tags (Org.OrgDoc blocks sections)) = do
  H.li ! A.class_ "my-2" $ do
    H.div ! A.class_ ("py-1 text-xl cursor-default " <> "hover:" <> itemHoverClass) $ do
      renderWordsList heading
      forM_ tags renderTag
      whenNotNull blocks $ \_ -> do
        H.div ! A.class_ "border-l-2 pl-2 text-gray-700 bg-gray-50 mt-2" $
          forM_ blocks renderBlock
    renderOrgSections sections

itemHoverClass :: H.AttributeValue
itemHoverClass = "bg-purple-100"

renderTag :: Text -> H.Html
renderTag tag =
  H.span
    ! A.class_ "border-1 p-0.5 bg-purple-200 font-bold rounded"
    ! A.title "Tag"
    $ H.toMarkup tag

renderWordsList :: Foldable f => f Org.Words -> H.Markup
renderWordsList =
  mapM_ $ \s -> renderWords s >> " "

renderWords :: Org.Words -> H.Markup
renderWords = \case
  Org.Bold s -> H.b (H.toHtml s)
  Org.Italic s -> H.i (H.toHtml s)
  Org.Highlight s -> H.mark (H.toHtml s)
  Org.Underline s -> H.u (H.toHtml s)
  Org.Verbatim s -> H.code (H.toHtml s)
  Org.Strike s -> H.del (H.toHtml s)
  Org.Link (Org.URL url) ms ->
    H.a ! A.href (hrefAttr url) $ maybe "" H.toHtml ms
  Org.Image (Org.URL url) ->
    H.img ! A.src (hrefAttr url)
  Org.Tags tags ->
    forM_ tags renderTag
  Org.Punct c ->
    H.toHtml c
  Org.Plain s ->
    -- `org-mode` library doedn't parse these; until then, we handle them here.
    if s `Set.member` Set.fromList ["TODO", "DONE"]
      then
        H.span
          ! A.class_ "border-1 p-0.5 bg-gray-600 text-white"
          ! A.title "Keyword"
          $ H.toMarkup s
      else H.toMarkup s
  where
    hrefAttr url =
      fromString . toString $ url

renderAST :: Show a => Text -> a -> H.Html
renderAST name x = do
  H.div ! A.class_ "border-2 p-2 rounded text-xs" $ do
    H.header ! A.class_ "font-bold" $ H.toMarkup name
    H.pre ! A.class_ "" $ H.toMarkup $ Shower.shower x
