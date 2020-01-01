{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Search
  ( view'
  ) where

import           Bytes                             (bytesToText)
import           Data.ByteString                   (ByteString)
import           Data.Foldable                     as Foldable
import           Data.Int                          (Int32)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Sequence                     as S
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.UUID                         as UUID
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector
import           Event                             (Event (..))
import           FoundationDB.Layer.Tuple          (Elem)
import qualified FoundationDB.Layer.Tuple          as LT
import           GI.Gtk                            (Align (..), Box (..),
                                                    Button (..), Entry (..),
                                                    Grid (..), Label (..),
                                                    Orientation (..),
                                                    ScrolledWindow (..),
                                                    entryGetText)
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Container.Grid
import           State                             (Search (..),
                                                    SearchRange (..),
                                                    SearchResult (..),
                                                    SearchResults (..),
                                                    maxKeyTupleSize,
                                                    maxValueTupleSize)
import           Text.Printf                       (printf)

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
    , GridChild
        { properties = defaultGridChildProperties {topAttach = 4, width = 2}
        , child = statusbar searchResults
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
    SearchSuccess _duration rows ->
      let keyWidth = fromMaybe 1 $ maxKeyTupleSize rows
          valueWidth = fromMaybe 1 $ maxValueTupleSize rows
       in bin ScrolledWindow [#hexpand := True, #vexpand := True] $
          container Grid [#hexpand := True, #columnSpacing := 12, #margin := 4] $
          Vector.concatMap (resultRow keyWidth valueWidth) $
          Vector.fromList $ zip [0 ..] (Foldable.toList rows)

resultRow ::
     Integer -> Integer -> (Int32, SearchResult) -> Vector (GridChild Event)
resultRow keyWidth valueWidth (rowN, SearchResult {..}) =
  Vector.fromList $ keyCells <> [eqCell] <> valueCells
  where
    eqCell :: GridChild Event
    eqCell =
      GridChild
        { properties =
            defaultGridChildProperties
              {topAttach = rowN, leftAttach = fromIntegral keyWidth}
        , child = widget Label [#label := "<b>=</b>", #useMarkup := True]
        }
    keyCells :: [GridChild Event]
    keyCells =
      case resultKey of
        (t, Nothing) -> [rawCell keyWidth 0 t]
        (_, Just ts) -> zipWith (\i t -> elemCell i (tupleHelp i) t) [0 ..] ts
    valueCells :: [GridChild Event]
    valueCells =
      case resultValue of
        (t, Nothing) -> [rawCell valueWidth (keyWidth + 1) t]
        (_, Just ts) ->
          zipWith
            (\i t -> elemCell (keyWidth + 1 + i) (tupleHelp i) t)
            [0 ..]
            ts
    rawCell :: Integer -> Integer -> ByteString -> GridChild Event
    rawCell width leftAttach label =
      GridChild
        { properties =
            defaultGridChildProperties
              { topAttach = rowN
              , leftAttach = fromIntegral leftAttach
              , width = fromIntegral width
              }
        , child =
            widget
              Label
              [ #label := trim (bytesToText label)
              , #tooltipText := "Raw binary data (can't decode as tuple)"
              , #halign := AlignStart
              ]
        }
    elemCell :: Integer -> Text -> Elem -> GridChild Event
    elemCell leftAttach tooltipPrefix elm =
      GridChild
        { properties =
            defaultGridChildProperties
              { topAttach = rowN
              , leftAttach = fromIntegral leftAttach
              , width = 1
              }
        , child = elemToWidget tooltipPrefix elm
        }

elemToWidget :: Text -> Elem -> Widget Event
elemToWidget tooltipPrefix =
  \case
    LT.None -> w "null" "null value"
    LT.Bytes bs -> w (trim $ bytesToText bs) "binary data"
    LT.Text t -> w (trim t) "text"
    LT.Int i -> w (T.pack $ show i) "integer"
    LT.Float f -> w (T.pack $ show f) "float"
    LT.Double d -> w (T.pack $ show d) "double"
    LT.Bool b -> w (T.pack $ show b) "bool"
    LT.UUID a b c d -> w (UUID.toText $ UUID.fromWords a b c d) "UUID"
    LT.CompleteVS vs -> w (T.pack $ show vs) "complete versionstamp"
    LT.IncompleteVS vs -> w (T.pack $ show vs) "incomplete versionstamp"
    LT.Tuple es ->
      container
        Box
        [#hexpand := True]
        (Vector.fromList $ zipWith tupleChild [0 ..] es)
  where
    w :: Text -> Text -> Widget Event
    w label tooltip =
      widget
        Label
        [ #label := label
        , #tooltipText := (tooltipPrefix <> tooltip)
        , #halign := AlignStart
        ]
    tupleChild :: Integer -> Elem -> BoxChild Event
    tupleChild i e =
      BoxChild
        { child = elemToWidget (tooltipPrefix <> tupleHelp i) e
        , properties = defaultBoxChildProperties {expand = True, fill = True}
        }

statusbar :: SearchResults -> Widget Event
statusbar res = widget Label [#label := label, #halign := AlignStart]
  where
    label
      | SearchSuccess duration rows <- res =
        let nRows = show $ S.length rows
            dur = printf "%.3fs" (realToFrac duration :: Double)
         in T.pack $ "Fetched " <> nRows <> " keys in " <> dur
      | otherwise = ""

tupleHelp :: Integer -> Text
tupleHelp index = "tuple item #" <> T.pack (show index) <> " -> "

escapeSyntaxHelp :: Text
escapeSyntaxHelp =
  "Text will be UTF-8 encoded except for byte values specified in hex, like '\\xA0'. Use double slash '\\\\' to enter a single slash."

trim :: Text -> Text
trim = T.replace "\n" " " . T.replace "\r" " " . trim'
  where
    trim' s
      | T.length s <= 50 = s
      | otherwise = T.take 47 s <> "..."
