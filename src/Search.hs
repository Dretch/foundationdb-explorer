module Search
  ( view'
  ) where

import Data.Foldable as Foldable
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Text (Text)
import GI.Gtk (Align (..), Box (..), Button (..), Entry (..), Label (..), ListBox (..), ListBoxRow (..), Orientation (..))
import GI.Gtk.Declarative

import Event (Event (..))
import qualified State

view' :: State.Search -> Widget Event
view' State.Search{searchRange = searchRange@State.SearchRange{..}, ..} =
    container Box [#orientation := OrientationVertical, #margin := 10, #spacing := 10]
      ([ keySelector "From" searchFrom
       , keySelector "To" searchTo
       , boxChild (widget Button ([#label := "Fetch", #halign := AlignEnd, on #clicked (StartSearch searchRange), #sensitive := not searchInProgress]))
       ] <> results)
    where
      results :: Vector (BoxChild Event)
      results = case searchResults of
        Nothing -> []
        Just rows ->
          [BoxChild
            defaultBoxChildProperties{expand = True, fill = True}
            (container ListBox [] (Vector.fromList $ Foldable.toList $ resultRow <$> rows))]

      resultRow :: State.SearchResult -> Bin ListBoxRow Event
      resultRow State.SearchResult{..} =
          bin ListBoxRow [] $ widget Label [#label := (resultKey <> " = " <> resultValue)]

      keySelector :: Text -> Text -> BoxChild Event
      keySelector label key =
          boxChild (container Box [#spacing := 10] [boxChild labelWidget, boxChild textWidget])
          where
              labelWidget = widget Label [#label := label]
              textWidget = widget Entry [#text := key, #sensitive := not searchInProgress]

boxChild :: Widget e -> BoxChild e
boxChild = BoxChild defaultBoxChildProperties