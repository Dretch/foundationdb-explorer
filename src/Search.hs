module Search
  ( view'
  ) where

import Data.Foldable as Foldable
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Text as Text
import GI.Gtk (Align (..), Box (..), Button (..), Entry (..), Label (..), ListBox (..), ListBoxRow (..), Orientation (..), entryGetText)
import GI.Gtk.Declarative

import Event (Event (..))
import State (Search (..), SearchRange (..), SearchResult (..), SearchResults (..))

view' :: Search -> Widget Event
view' Search{searchRange = searchRange@SearchRange{..}, ..} =
    container Box [#orientation := OrientationVertical, #margin := 10, #spacing := 10]
      ([ keySelector "From" searchFrom (\t -> searchRange{searchFrom = t})
       , keySelector "To" searchTo (\t-> searchRange{searchTo = t})
       , boxChild (widget Button ([#label := "Fetch", #halign := AlignEnd, on #clicked StartSearch, #sensitive := activateInputs]))
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

      resultRow :: SearchResult -> Bin ListBoxRow Event
      resultRow SearchResult{..} =
          bin ListBoxRow [] $ widget Label [#label := (trim resultKey <> " = " <> trim resultValue)]

      keySelector :: Text -> Text -> (Text -> SearchRange) -> BoxChild Event
      keySelector label key updateRange =
          boxChild (container Box [#spacing := 10] [labelWidget, textWidget])
          where
              labelWidget =
                boxChild $ widget Label [#label := label]

              textWidget =
                BoxChild defaultBoxChildProperties{expand = True, fill = True} $
                  widget Entry [#text := key, onM #changed onChange, #sensitive := activateInputs] -- todo: tooltip explaining \x## syntax
              
              onChange entry = do
                text <- entryGetText entry
                pure $ SetSearchRange $ updateRange text
      
      activateInputs :: Bool
      activateInputs = searchResults /= SearchInProgress

boxChild :: Widget e -> BoxChild e
boxChild = BoxChild defaultBoxChildProperties

trim :: Text -> Text
trim s
   | Text.length s <= 100 = s
   | otherwise            = Text.take 97 s <> "..."