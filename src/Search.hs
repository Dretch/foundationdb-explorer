module Search
  ( view'
  ) where

import Data.Foldable as Foldable
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Text as Text
import GI.Gtk (Align (..), Box (..), Button (..), Entry (..), Label (..), ListBox (..), ListBoxRow (..), Orientation (..))
import GI.Gtk.Declarative

import Event (Event (..))
import qualified State -- todo: rm?
import State (SearchResults (..))

view' :: State.Search -> Widget Event
view' State.Search{searchRange = searchRange@State.SearchRange{..}, ..} =
    container Box [#orientation := OrientationVertical, #margin := 10, #spacing := 10]
      ([ keySelector "From" searchFrom
       , keySelector "To" searchTo
       , boxChild (widget Button ([#label := "Fetch", #halign := AlignEnd, on #clicked (StartSearch searchRange), #sensitive := activateInputs]))
       ] <> results)
    where
      results :: Vector (BoxChild Event)
      results = case searchResults of
        SearchNotStarted -> []
        SearchInProgress -> []
        SearchFailure msg ->
          [boxChild $ widget Label [#label := ("Search failed: " <> msg)]]
        SearchSuccess rows ->
          [BoxChild
            defaultBoxChildProperties{expand = True, fill = True}
            (container ListBox [] (Vector.fromList $ Foldable.toList $ resultRow <$> rows))]

      resultRow :: State.SearchResult -> Bin ListBoxRow Event
      resultRow State.SearchResult{..} =
          bin ListBoxRow [] $ widget Label [#label := (trim resultKey <> " = " <> trim resultValue)]

      keySelector :: Text -> Text -> BoxChild Event
      keySelector label key =
          boxChild (container Box [#spacing := 10] [boxChild labelWidget, boxChild textWidget])
          where
              labelWidget = widget Label [#label := label]
              textWidget = widget Entry [#text := key, #sensitive := activateInputs] -- todo: tooltip explaining \x## syntax
      
      activateInputs :: Bool
      activateInputs = searchResults /= SearchInProgress

boxChild :: Widget e -> BoxChild e
boxChild = BoxChild defaultBoxChildProperties

trim :: Text -> Text
trim s
   | Text.length s <= 100 = s
   | otherwise            = Text.take 97 s <> "..."