{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Search
  ( view'
  ) where

import           Data.Foldable                     as Foldable
import           Data.Int                          (Int32)
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           GI.Gtk                            (Align (..), Button (..),
                                                    Entry (..), Grid (..),
                                                    Label (..),
                                                    Orientation (..),
                                                    ScrolledWindow (..),
                                                    entryGetText)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Container.Grid

import           Event                             (Event (..))
import           State                             (Search (..),
                                                    SearchRange (..),
                                                    SearchResult (..),
                                                    SearchResults (..),
                                                    maxKeyTupleSize,
                                                    maxValueTupleSize)

view' :: Search -> Widget Event
view' Search {searchRange = searchRange@SearchRange {..}, ..} =
  container
    Grid
    [ #orientation := OrientationVertical
    , #margin := 4
    , #rowSpacing := 4
    , #columnSpacing := 4
    ]
    [ GridChild
        { properties = defaultGridChildProperties
        , child = widget Label [#label := "From", #halign := AlignEnd]
        }
    , GridChild
        { properties = defaultGridChildProperties {leftAttach = 1}
        , child =
            widget
              Entry
              [ #text := searchFrom
              , #tooltipText := escapeSyntaxHelp
              , onM #changed $ onChange (\t -> searchRange {searchFrom = t})
              , #sensitive := activateInputs
              , #hexpand := True
              ]
        }
    , GridChild
        { properties = defaultGridChildProperties {topAttach = 1}
        , child = widget Label [#label := "To", #halign := AlignEnd]
        }
    , GridChild
        { properties =
            defaultGridChildProperties {topAttach = 1, leftAttach = 1}
        , child =
            widget
              Entry
              [ #text := searchTo
              , #tooltipText := escapeSyntaxHelp
              , onM #changed $ onChange (\t -> searchRange {searchTo = t})
              , #sensitive := activateInputs
              , #hexpand := True
              ]
        }
    , GridChild
        { properties = defaultGridChildProperties {topAttach = 2, width = 2}
        , child =
            widget
              Button
              [ #label := "Fetch"
              , #halign := AlignEnd
              , on #clicked StartSearch
              , #sensitive := activateInputs
              ]
        }
    , GridChild
        { properties = defaultGridChildProperties {topAttach = 3, width = 2}
        , child = results searchResults
        }
    ]
  where
    onChange :: (Text -> SearchRange) -> Entry -> IO Event
    onChange updateRange entry = do
      text <- entryGetText entry
      pure $ SetSearchRange $ updateRange text
    activateInputs :: Bool
    activateInputs = searchResults /= SearchInProgress

results :: SearchResults -> Widget Event
results =
  \case
    SearchNotStarted -> widget Label []
    SearchInProgress -> widget Label [#label := "Loading..."]
    SearchFailure msg -> widget Label [#label := ("Search failed: " <> msg)]
    SearchSuccess rows ->
      let keyWidth = fromMaybe 1 $ maxKeyTupleSize rows
          valueWidth = fromMaybe 1 $ maxValueTupleSize rows
       in bin ScrolledWindow [#hexpand := True, #vexpand := True] $
          container Grid [#hexpand := True, #columnSpacing := 12, #margin := 4] $
          Vector.concatMap (resultRow keyWidth valueWidth) $
          Vector.fromList $ zip [0 ..] (Foldable.toList rows)

resultRow ::
     Integer -> Integer -> (Int32, SearchResult) -> Vector (GridChild Event)
resultRow keyWidth valueWidth (rowN, SearchResult {..}) =
  keyCells <> [eqCell] <> valueCells
  where
    eqCell :: GridChild Event
    eqCell =
      GridChild
        { properties =
            defaultGridChildProperties
              {topAttach = rowN, leftAttach = fromIntegral keyWidth}
        , child = widget Label [#label := "<b>=</b>", #useMarkup := True]
        }
    keyCells :: Vector (GridChild Event)
    keyCells =
      case resultKey of
        (t, Nothing) -> [cell keyWidth 0 t]
        (_, Just ts) -> Vector.fromList $ uncurry (cell 1) <$> zip [0 ..] ts
    valueCells :: Vector (GridChild Event)
    valueCells =
      case resultValue of
        (t, Nothing) -> [cell valueWidth (keyWidth + 1) t]
        (_, Just ts) ->
          Vector.fromList $ uncurry (cell 1) <$> zip [keyWidth + 1 ..] ts
    cell :: Integer -> Integer -> Text -> GridChild Event
    cell width leftAttach label =
      GridChild
        { properties =
            defaultGridChildProperties
              { topAttach = rowN
              , leftAttach = fromIntegral leftAttach
              , width = fromIntegral width
              }
        , child = widget Label [#label := trim label, #halign := AlignStart]
        }

escapeSyntaxHelp :: Text
escapeSyntaxHelp =
  "Text will be UTF-8 encoded except for byte values specified in hex, like '\\xA0'. Use double slash '\\\\' to enter a single slash."

trim :: Text -> Text
trim = T.replace "\n" " " . T.replace "\r" " " . trim'
  where
    trim' s
      | T.length s <= 50 = s
      | otherwise = T.take 47 s <> "..."
