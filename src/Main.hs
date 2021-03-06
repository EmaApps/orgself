{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Set as Set
import Data.Tagged (Tagged (Tagged), untag)
import Data.Time
import Data.Time.Extra
import Ema (Ema)
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as FileSystem
import qualified Ema.Helper.Tailwind as Tailwind
import OrgSelf.Data (Diary)
import qualified OrgSelf.Data as Data
import OrgSelf.Data.Measure
import qualified OrgSelf.Data.Tag as Data
import qualified OrgSelf.Data.Task as Task
import OrgSelf.Route
import qualified OrgSelf.Widget.Icons as Icons
import qualified Shower
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = do
  Ema.runEma render $ \model -> do
    FileSystem.mountFileSystemOnLVar "." ["*.org"] model $ \fp -> \case
      FileSystem.Update -> do
        mOrgs <- Data.parseDailyNote fp
        pure $ maybe id (Data.diaryUpdate . uncurry Data.DiaryModify) mOrgs
      FileSystem.Delete ->
        pure $ maybe id (Data.diaryUpdate . Data.DiaryDel) $ Data.parseDailyNoteFilepath fp

render :: Ema.CLI.Action -> Diary -> Route -> LByteString
render emaAction diary r = do
  let title = case r of
        Index -> "My Diary"
        OnDay d -> toText (formatDay d) <> " – My Diary"
        Tag s -> "#" <> untag s <> " – My Diary"
  Tailwind.layout emaAction (H.title $ H.text title) $
    H.div ! A.class_ "container mx-auto" $ do
      case r of
        Index -> do
          heading "My Diary"
          renderDiaryListing (Data.diaryCal diary)
          H.header "Tags"
          forM_ (Map.toList $ Data.diaryTags diary) $ \(tag, length -> n) ->
            H.li $ do
              renderTag tag
              " (" <> show n <> ")"
        OnDay day -> do
          renderWeekNav diary day
          maybe "not found" renderDay (Map.lookup day $ Data.diaryCal diary)
        Tag tag -> do
          heading $ H.toHtml $ "Days tagged with #" <> untag tag
          routeElem Index "Back to Index"
          case Map.lookup tag (Data.diaryTags diary) of
            Nothing -> "Tag not found"
            Just days -> do
              let tagCal = Map.filterWithKey (\day _ -> Set.member day days) (Data.diaryCal diary)
              renderDiaryListing tagCal
      H.footer
        ! A.class_ "text-xs my-4 py-2 text-center bg-gray-200"
        $ do
          "Powered by "
          H.a
            ! A.href "https://github.com/srid/orgself"
            ! A.target "blank_"
            ! A.class_ "text-purple font-bold"
            $ "orgself"

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%h %d %a"

renderWeekNav :: Diary -> Day -> H.Html
renderWeekNav diary day = do
  let a = firstDayOfWeekOnAfter Monday (addDays (-6) day)
      b = firstDayOfWeekOnAfter Sunday day
      prev = addDays (-1) a
      next = addDays 1 b
  H.div ! A.class_ "flex justify-evenly flex-wrap border-4 border-purple-600 bg-purple-50 my-2 text-xl shadow" $ do
    let item = H.span ! A.class_ "m-2"
    item $ routeElem Index "🏠"
    whenJust (Data.diaryLookup prev diary) $ \_ ->
      item $ routeElem (OnDay prev) "←"
    item ! A.title "Out of 00-53 weeks in a year" $ do
      "W"
      H.toHtml $ formatTime defaultTimeLocale "%V" day
    forM_ [a .. b] $ \oDay ->
      item $ do
        let s = formatDay oDay
        if Map.member oDay (Data.diaryCal diary)
          then
            if oDay == day
              then H.span ! A.class_ "font-bold" $ H.toMarkup s
              else routeElem (OnDay oDay) $ H.toMarkup s
          else H.span ! A.class_ "text-gray-400" $ H.toMarkup s
        whenNotNull (Data.diaryLookupTasks oDay diary) $ \(toList -> tasks) -> do
          let todos = filter ((== Org.TODO) . Task.taskState) tasks
          unless (null todos) $ do
            H.sup
              ! A.class_ "font-bold text-red-500"
              ! A.title (show (length todos) <> " TODOs on this day")
              $ H.text (show $ length todos)
    whenJust (Data.diaryLookup next diary) $ \_ ->
      item $ routeElem (OnDay next) "→"

routeDay :: Day -> H.Html
routeDay day =
  routeElem (OnDay day) $ H.toMarkup @Text (show day)

routeElem :: Ema a r => r -> H.Html -> H.Html
routeElem r' = H.a ! A.class_ "text-purple-500 hover:underline" ! routeHref r'

heading :: H.Html -> H.Html
heading =
  H.header
    ! A.class_ "text-4xl my-2 py-2 font-bold text-center bg-purple-600 text-gray-100 shadow"

renderDiaryListing :: Map Day (OrgFile, Measures) -> H.Html
renderDiaryListing cal = do
  H.div ! A.class_ "flex flex-col" $ do
    let usedMeasures = Set.toList $ Set.fromList $ concat $ Map.elems cal <&> \(_, measures) -> DMap.keys measures
        td cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)
        th cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)
    -- Render diary index along with self-tracking measures, as a table.
    H.table ! A.class_ "table-auto" $ do
      H.tr ! A.class_ "bg-gray-50" $ do
        th "" ""
        forM_ usedMeasures $ \m -> do
          th "font-bold" $ H.toHtml $ measureName m
      forM_ (sortOn (Down . fst) $ Map.toList cal) $ \(day, (_, dayMeasures)) -> do
        H.tr $ do
          th "font-bold" $ routeDay day
          forM_ (flip lookupMeasure dayMeasures <$> usedMeasures) $ \case
            Nothing -> td "" ""
            Just measure -> td "" $ renderMeasureValue measure

routeHref :: Ema a r => r -> H.Attribute
routeHref r' =
  A.href (fromString . toString $ Ema.routeUrl r')

renderDay :: (OrgFile, Measures) -> H.Html
renderDay (orgFile, measures) = do
  renderMeasures measures
  renderOrg orgFile

-- TODO: Move to separate module (after decoupling tailwind styling)
-- Even make it a separate library, as long as CSS classes can be customized!
renderOrg :: OrgFile -> H.Html
renderOrg _org@(Org.OrgFile _meta doc) = do
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
    replicateM_ y $ Icons.star "inline h-5 w-6 text-gray-300"
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
  Org.Code mlang s -> do
    H.div $ H.em $ show mlang
    H.code $ H.pre ! A.class_ (show mlang <> " bg-black text-gray-100 w-full border-l-2 pl-2") $ H.text s
  Org.List (Org.ListItems items) ->
    H.ul ! A.class_ "list-disc ml-4" $
      forM_ items $ \(Org.Item ws msub) ->
        H.li $ do
          renderWordsList ws
          whenJust msub $ \listItems ->
            renderBlock $ Org.List listItems
  Org.Table _rows -> renderAST "TODO: Table" blk
  Org.Paragraph ws -> H.p ! A.class_ "py-1" $ renderWordsList ws

renderSection :: Org.Section -> H.Html
renderSection Org.Section {..} = do
  let tooltip = case sectionClosed of
        Nothing -> ""
        Just orgTime -> "Closed on: " <> show orgTime
  H.li ! A.class_ "my-2" $ do
    H.div ! A.title tooltip ! A.class_ "py-1 cursor-default hover:bg-purple-100" $ do
      whenJust sectionTodo $ \todoState ->
        renderTodoState todoState
      H.span $ renderWordsList sectionHeading
      whenNotNull sectionTags $ \_ ->
        H.span ! A.class_ "border-l-2 pl-1 ml-2 inline-flex space-x-2 justify-center items-center" $
          forM_ sectionTags $ renderTag . Tagged
      H.div ! A.class_ "float-right" $ do
        -- TODO: Deadline etc
        -- let x = sectionDeadline
        unless (Map.null sectionProps) $ do
          H.pre $ H.toHtml $ Shower.shower sectionProps
      whenNotNull (Org.docBlocks sectionDoc) $ \blocks -> do
        H.div ! A.class_ "border-l-2 pl-2 text-gray-700 bg-gray-50 mt-2" $
          forM_ blocks renderBlock
    renderOrgSections $ Org.docSections sectionDoc

renderTag :: Data.Tag -> H.Html
renderTag tag =
  H.a
    ! A.class_ "bg-gray-200 font-mono text-xs rounded hover:bg-gray-100"
    ! A.title "Tag"
    ! routeHref (Tag tag)
    $ H.toMarkup (untag tag)

renderWordsList :: Foldable f => f Org.Words -> H.Markup
renderWordsList =
  mapM_ $ \s -> do
    renderWords s
    case s of
      -- HACK: Not sure why org-mode parser is parsing punctuations. They seem
      -- useless. We don't want to separate punctuations by whitespace,
      -- especially when it is not in the source document. This hack achieves
      -- that.
      Org.Punct _ -> pure ()
      _ -> " "

renderWords :: Org.Words -> H.Markup
renderWords = \case
  Org.Bold s -> H.b (H.toHtml s)
  Org.Italic s -> H.i (H.toHtml s)
  Org.Highlight s -> H.mark (H.toHtml s)
  Org.Underline s -> H.u (H.toHtml s)
  Org.Verbatim s -> H.code (H.toHtml s)
  Org.Strike s -> H.del (H.toHtml s)
  Org.Link (Org.URL url) ms ->
    H.a ! A.class_ "text-purple-700 hover:font-bold" ! A.href (hrefAttr url) $ maybe "" H.toHtml ms
  Org.Image (Org.URL url) ->
    H.img ! A.src (hrefAttr url)
  Org.Punct c ->
    H.toHtml c
  Org.Plain s ->
    H.text s
  where
    hrefAttr url =
      fromString . toString $ url

renderAST :: Show a => Text -> a -> H.Html
renderAST name x = do
  H.div ! A.class_ "border-2 p-2 rounded text-xs" $ do
    H.header ! A.class_ "font-bold" $ H.toMarkup name
    H.pre ! A.class_ "" $ H.toMarkup $ Shower.shower x

renderTodoState :: Org.Todo -> H.Markup
renderTodoState todoState = do
  let specialWordColors =
        Map.fromList
          [ (Org.TODO, "bg-red-600"),
            (Org.DONE, "bg-green-600")
          ]
  H.span ! A.class_ "mr-1" $ do
    case Map.lookup todoState specialWordColors of
      Nothing -> H.toMarkup $ H.text $ show todoState
      Just clr ->
        H.span
          ! A.class_ ("border-1 p-0.5 text-white " <> clr)
          $ H.toMarkup $ H.text $ show todoState
