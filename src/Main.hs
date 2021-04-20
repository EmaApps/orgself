{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async (race_)
import qualified Data.LVar as LVar
import qualified Data.Map.Strict as Map
import Data.Org (OrgFile)
import qualified Data.Org as Org
import qualified Data.Set as Set
import Ema.App (runEma)
import qualified Ema.Layout as Layout
import Ema.Route (IsRoute (routeUrl))
import Memoir.Data (Diary)
import qualified Memoir.Data as Data
import Memoir.Route (Route (..))
import qualified Shower
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = mainWith . drop 1 =<< getArgs

mainWith :: [String] -> IO ()
mainWith args = do
  folder <- case args of
    [path] -> canonicalizePath path
    _ -> canonicalizePath "example"
  model <- LVar.new =<< Data.diaryFrom folder
  race_
    (runEma model render)
    (Data.watchAndUpdateDiary folder model)
  where
    render (diary :: Diary) (r :: Route) =
      Layout.tailwindSite (H.title "My Diary") $
        H.div ! A.class_ "container mx-auto" $ do
          let heading =
                H.header
                  ! A.class_ "text-4xl my-2 py-2 font-bold text-center bg-purple-600 text-gray-100 shadow"
          case r of
            Index -> do
              heading "My Diary"
              H.div ! A.class_ "" $
                forM_ (sortOn Down $ Map.keys diary) $ \day ->
                  H.li $ routeElem (OnDay day) $ H.toMarkup @Text (show day)
            OnDay day -> do
              heading $ show day
              routeElem Index "Back to Index"
              maybe "not found" renderOrg (Map.lookup day diary)
          H.footer
            ! A.class_ "text-xs my-4 py-2 text-center bg-gray-200"
            $ do
              "Powered by "
              H.a
                ! A.href "https://github.com/srid/memoir"
                ! A.target "blank_"
                ! A.class_ "text-purple font-bold"
                $ "memoir"
    routeElem r w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r $ w
    routeHref r =
      A.href (fromString . toString $ routeUrl r)

renderOrg :: OrgFile -> H.Html
renderOrg _org@(Org.OrgFile meta doc) = do
  let heading = H.header ! A.class_ "text-2xl my-2 font-bold"
  unless (null meta) $ do
    heading "Meta"
    renderMeta meta
  heading "Doc"
  -- Debug dump
  -- H.pre $ H.toMarkup (Shower.shower org)
  renderOrgDoc doc

renderMeta :: Map Text Text -> H.Html
renderMeta meta = do
  H.table ! A.class_ "table-auto" $ do
    let td cls = H.td ! A.class_ ("border px-4 py-2 " <> cls)
    forM_ (Map.toList meta) $ \(k, v) ->
      H.tr $ do
        td "font-bold" $ H.toMarkup k
        td "font-mono" $ H.toMarkup v

renderOrgDoc :: Org.OrgDoc -> H.Html
renderOrgDoc (Org.OrgDoc blocks sections) = do
  H.ul ! A.class_ "list-disc ml-8" $ do
    whenNotNull blocks $ \_ -> do
      H.header ! A.class_ "text-2xl font-bold" $ "Blocks"
      H.pre $ H.toMarkup (Shower.shower blocks)
    whenNotNull sections $ \_ -> do
      forM_ sections renderSection

renderSection :: Org.Section -> H.Html
renderSection (Org.Section heading tags doc) = do
  H.li ! A.class_ "my-2" $ do
    H.span ! A.class_ "py-1 text-xl hover:bg-purple-100 cursor-default" $ do
      forM_ heading $ \s ->
        renderWords s >> " "
      forM_ tags renderTag
    renderOrgDoc doc

renderTag :: Text -> H.Html
renderTag tag =
  H.span
    ! A.class_ "border-1 p-0.5 bg-purple-200 font-bold rounded"
    ! A.title "Tag"
    $ H.toMarkup tag

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
