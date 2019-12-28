module Search
  ( view'
  ) where

import Data.Int (Int32)
import Data.Foldable as Foldable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Text (Text)
import qualified Data.Text as Text
import GI.Gtk (Align (..), Button (..), Entry (..), Grid (..), Label (..), Orientation (..), entryGetText)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.Container.Grid

import Event (Event (..))
import State (Search (..), SearchRange (..), SearchResult (..), SearchResults (..))

view' :: Search -> Widget Event
view' Search{searchRange = searchRange@SearchRange{..}, ..} =
    container Grid [#orientation := OrientationVertical, #margin := 4, #rowSpacing := 4, #columnSpacing := 4]
      [ GridChild {
          properties = defaultGridChildProperties,
          child = widget Label [#label := "From", #halign := AlignEnd]
        },
        GridChild {
          properties = defaultGridChildProperties{leftAttach = 1},
          child = widget Entry [#text := searchFrom, onM #changed $ onChange (\t -> searchRange{searchFrom = t}), #sensitive := activateInputs, #hexpand := True]
        },
        GridChild {
          properties = defaultGridChildProperties{topAttach = 1},
          child = widget Label [#label := "To", #halign := AlignEnd]
        },
        GridChild {
          properties = defaultGridChildProperties{topAttach = 1, leftAttach = 1},
          child = widget Entry [#text := searchTo, onM #changed $ onChange (\t -> searchRange{searchTo = t}), #sensitive := activateInputs, #hexpand := True]
        },
        GridChild {
          properties = defaultGridChildProperties{topAttach = 2, width = 2},
          child = widget Button [#label := "Fetch", #halign := AlignEnd, on #clicked StartSearch, #sensitive := activateInputs]
        },
        GridChild {
          properties = defaultGridChildProperties{topAttach = 3, width = 2},
          child = results
        }
      ]
    where
      results :: Widget Event
      results = case searchResults of
        SearchNotStarted ->
          widget Label []
        SearchInProgress ->
          widget Label [#label := "Loading..."]
        SearchFailure msg ->
          widget Label [#label := ("Search failed: " <> msg)]
        SearchSuccess rows ->
          container Grid [#hexpand := True, #columnSpacing := 4] $
             (Vector.concatMap resultRow $ Vector.fromList $ zip [0..] (Foldable.toList rows))

      resultRow :: (Int32, SearchResult) -> Vector (GridChild Event)
      resultRow (rowN, SearchResult{..}) =
        [ GridChild {
            properties = defaultGridChildProperties{topAttach = rowN},
            child = widget Label [#label := trim resultKey, #halign := AlignStart]
          },
          GridChild {
            properties = defaultGridChildProperties{topAttach = rowN, leftAttach = 1},
            child = widget Label [#label := "="]
          },
          GridChild {
            properties = defaultGridChildProperties{topAttach = rowN, leftAttach = 2},
            child = widget Label [#label := trim resultValue, #hexpand := True, #halign := AlignStart]
          }
        ]

      onChange :: (Text -> SearchRange) -> Entry -> IO Event
      onChange updateRange entry = do
        text <- entryGetText entry
        pure $ SetSearchRange $ updateRange text
      
      activateInputs :: Bool
      activateInputs = searchResults /= SearchInProgress

trim :: Text -> Text
trim s
   | Text.length s <= 100 = s
   | otherwise            = Text.take 97 s <> "..."