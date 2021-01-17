{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module FDBE.Component.Search
  ( Search(..)
  ) where

import           FDBE.Prelude

import           Control.Exception                           (displayException)
import           Control.Monad.State.Class                   (get, gets, modify)
import           Data.Foldable                               as Foldable
import qualified Data.Sequence                               as S
import qualified Data.Text                                   as T
import           Data.Time                                   (NominalDiffTime, UTCTime)
import           Data.Time.Clock                             (getCurrentTime)
import qualified Data.UUID                                   as UUID
import qualified Data.Vector                                 as Vector
import           FoundationDB (Database)
import qualified FoundationDB.Layer.Tuple                    as LT
import           FoundationDB.Layer.Tuple                    (Elem)
import           FoundationDB.Versionstamp                   (TransactionVersionstamp (..),
                                                              Versionstamp (..))
import           GI.Gtk                                      (Align (..),
                                                              Box (..),
                                                              Button (..),
                                                              CheckButton (..),
                                                              Frame (..),
                                                              Grid (..),
                                                              Label (..),
                                                              Orientation (..),
                                                              ScrolledWindow (..),
                                                              Window (..),
                                                              WindowPosition (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Attributes.Custom.Window (presentWindow,
                                                              window)
import           GI.Gtk.Declarative.Component
import           GI.Gtk.Declarative.Container.Grid

import           FDBE.Bytes                                  (bytesToText)
import           FDBE.FoundationDB                           (getSearchResult, SearchRange(..), SearchResult(..))
import           FDBE.State                                  (Operation (..))
import qualified FDBE.Component.TupleEntry                   as TupleEntry
import qualified FDBE.Component.IntegerSpinner               as IntegerSpinner

newtype Search event = Search Database

data SearchResults =
  SearchResults
    { searchDuration :: NominalDiffTime
    , searchSeq      :: Seq SearchResult
    , searchViewFull :: Maybe SearchResultsViewFull
    }
  deriving (Eq)

data SearchResultsViewFull =
  SearchResultsViewFull
    { viewFullText :: Text
    , viewFullTime :: UTCTime
    }
  deriving (Eq)

instance Component Search where

  data ComponentState Search = SearchState
    { searchRange   :: SearchRange
    , searchResults :: Operation SearchResults
    }
  
  data ComponentAction Search
    = SetSearchRange SearchRange
    | StartSearch
    | FinishSearch (Either Text (NominalDiffTime, Seq SearchResult))
    | SetSearchResultsViewFull (Maybe SearchResultsViewFull)

  createComponent (Search _) =
    ( SearchState 
        { searchRange =
            SearchRange
              { searchFrom = Left ""
              , searchTo = Left "\\xFF"
              , searchLimit = 100
              , searchReverse = False
              }
        , searchResults = OperationNotStarted
        }
    , Nothing
    )

  patchComponent state (Search _) =
    state
  
  update (Search database) = \case
    SetSearchRange range -> do
      modify $ \state -> state {searchRange = range}
    StartSearch -> do
      modify $ \state -> state {searchResults = OperationInProgress}
      SearchState{..} <- get
      updateIO $ do
        res <- getSearchResult database searchRange
        pure . Just . FinishSearch $ mapLeft (T.pack . displayException) res
    FinishSearch results' -> do
      let mkSuccess (searchDuration, searchSeq) =
            OperationSuccess SearchResults {searchViewFull = Nothing, ..}
          searchResults = either OperationFailure mkSuccess results'
      modify $ \state -> state {searchResults}
    SetSearchResultsViewFull viewFull -> do
      gets searchResults >>= \case
        OperationSuccess SearchResults {..} ->
          modify $ \state ->
            state { searchResults = OperationSuccess SearchResults {searchViewFull = viewFull, ..}}
        _ ->
          pure ()
  
  view (Search _) SearchState{searchRange = searchRange@SearchRange {..}, ..} =
    container
      Grid
      ([ #orientation := OrientationVertical
      , #margin := 4
      , #rowSpacing := 4
      , #columnSpacing := 4
      ] <> windows searchResults)
      [ GridChild
          { properties = defaultGridChildProperties
          , child =
              widget Label [#label := "From", #marginTop := 6, #halign := AlignEnd, #valign := AlignStart]
          }
      , GridChild
          { properties = defaultGridChildProperties {leftAttach = 1}
          , child = component TupleEntry.tupleEntry
              { TupleEntry.sensitive = activateInputs
              , TupleEntry.value = searchFrom
              , TupleEntry.onChanged = Just (\s -> SetSearchRange searchRange {searchFrom = s})
              }
          }
      , GridChild
          { properties = defaultGridChildProperties {topAttach = 1}
          , child =
              widget Label [#label := "To", #marginTop := 6, #halign := AlignEnd, #valign := AlignStart]
          }
      , GridChild
          { properties =
              defaultGridChildProperties {topAttach = 1, leftAttach = 1}
          , child = component TupleEntry.tupleEntry
                { TupleEntry.sensitive = activateInputs
                , TupleEntry.value = searchTo
                , TupleEntry.onChanged = Just (\s -> SetSearchRange searchRange {searchTo = s})
                }
          }
      , GridChild
          { properties = defaultGridChildProperties {topAttach = 2}
          , child = widget Label [#label := "Limit", #halign := AlignEnd]
          }
      , GridChild
          { properties =
              defaultGridChildProperties {topAttach = 2, leftAttach = 1}
          , child =
              component IntegerSpinner.integerSpinner
                { IntegerSpinner.rawAttributes = [#sensitive := activateInputs]
                , IntegerSpinner.value = searchLimit
                , IntegerSpinner.onChanged = Just (\v -> SetSearchRange searchRange { searchLimit = v })
                }
          }
      , GridChild
          { properties = defaultGridChildProperties {topAttach = 3, leftAttach = 1}
          , child =
              widget CheckButton
                [ #label := "Reverse Order"
                , #active := searchReverse
                , #sensitive := activateInputs
                , onM #toggled (fmap (\b -> SetSearchRange searchRange { searchReverse = b }) . #getActive)
                ]
          }
      , GridChild
          { properties = defaultGridChildProperties {topAttach = 4, width = 2}
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
          { properties = defaultGridChildProperties {topAttach = 5, width = 2}
          , child = results searchResults
          }
      , GridChild
          { properties = defaultGridChildProperties {topAttach = 6, width = 2}
          , child = statusbar searchResults
          }
      ]
    where
      activateInputs = searchResults /= OperationInProgress

windows :: Operation SearchResults -> Vector (Attribute widget (ComponentAction Search))
windows = \case
  OperationSuccess SearchResults { searchViewFull = Just res } -> [window () (mkWindow res)]
  _ -> []
  where
    mkWindow :: SearchResultsViewFull -> Bin Window (ComponentAction Search)
    mkWindow SearchResultsViewFull {..} =
      bin Window
        [ #widthRequest := 600
        , #heightRequest := 400
        , #windowPosition := WindowPositionCenter
        , #title := "View full text"
        , on #deleteEvent (const (True, SetSearchResultsViewFull Nothing))
        , presentWindow viewFullTime
        ]
        (bin ScrolledWindow
          [ #hexpand := True
          , #vexpand := True
          ]
          (widget Label
            [ #label := viewFullText
            , #halign := AlignStart
            , #valign := AlignStart
            , #margin := 4
            , #selectable := True
            ]
          )
        )

results :: Operation SearchResults -> Widget (ComponentAction Search)
results =
  \case
    OperationNotStarted -> widget Label []
    OperationInProgress -> widget Label [#label := "Loading..."]
    OperationFailure msg -> widget Label [#label := ("Search failed: " <> msg)]
    OperationSuccess SearchResults {searchSeq = rows} ->
      let keyWidth = fromMaybe 1 $ maxKeyTupleSize rows
          valueWidth = fromMaybe 1 $ maxValueTupleSize rows
       in bin ScrolledWindow [#hexpand := True, #vexpand := True] $
          container Grid [#hexpand := True] $
          Vector.concatMap (resultRow keyWidth valueWidth) $
          Vector.fromList $ zip [0 ..] (Foldable.toList rows)

resultRow ::
     Integer -> Integer -> (Int32, SearchResult) -> Vector (GridChild (ComponentAction Search))
resultRow keyWidth valueWidth (rowN, SearchResult {..}) =
  Vector.fromList $ keyCells <> [eqCell] <> valueCells
  where
    eqCell :: GridChild (ComponentAction Search)
    eqCell =
      GridChild
        { properties =
            defaultGridChildProperties
              {topAttach = rowN, leftAttach = fromIntegral keyWidth}
        , child = widget Label [#label := "=", classes ["equals-cell"]]
        }
    keyCells :: [GridChild (ComponentAction Search)]
    keyCells =
      case resultKey of
        (t, Nothing) -> [rawCell keyWidth 0 t]
        (_, Just ts) -> zipWith (\i t -> elemCell i (tupleHelp i) t) [0 ..] ts
    valueCells :: [GridChild (ComponentAction Search)]
    valueCells =
      case resultValue of
        (t, Nothing) -> [rawCell valueWidth (keyWidth + 1) t]
        (_, Just ts) ->
          zipWith
            (\i t -> elemCell (keyWidth + 1 + i) (tupleHelp i) t)
            [0 ..]
            ts
    rawCell :: Integer -> Integer -> ByteString -> GridChild (ComponentAction Search)
    rawCell width leftAttach label =
      GridChild
        { properties =
            defaultGridChildProperties
              { topAttach = rowN
              , leftAttach = fromIntegral leftAttach
              , width = fromIntegral width
              }
        , child =
            resultLabel
              rowN
              (bytesToText label)
              "Raw binary data (can't decode as tuple)"
        }
    elemCell :: Integer -> Text -> Elem -> GridChild (ComponentAction Search)
    elemCell leftAttach tooltipPrefix elm =
      GridChild
        { properties =
            defaultGridChildProperties
              { topAttach = rowN
              , leftAttach = fromIntegral leftAttach
              , width = 1
              }
        , child = elemToWidget rowN tooltipPrefix elm
        }

elemToWidget :: Int32 -> Text -> Elem -> Widget (ComponentAction Search)
elemToWidget rowN tooltipPrefix =
  \case
    LT.None -> w "null" "null value"
    LT.Bytes bs -> w (bytesToText bs) "binary data"
    LT.Text t -> w t "text"
    LT.Int i -> w (T.pack $ show i) "integer"
    LT.Float f -> w (T.pack $ show f) "float"
    LT.Double d -> w (T.pack $ show d) "double"
    LT.Bool b -> w (T.pack $ show b) "bool"
    LT.UUID a b c d -> w (UUID.toText $ UUID.fromWords a b c d) "UUID"
    LT.CompleteVS (CompleteVersionstamp (TransactionVersionstamp tx batch) user) ->
      w (T.pack $ printf "tx: %d, batch: %d, user: %d" tx batch user) "versionstamp"
    LT.IncompleteVS (IncompleteVersionstamp user) ->
      w (T.pack $ printf "user: %d" user) "incomplete versionstamp"
    LT.Tuple es ->
      container
        Box
        [#spacing := 2, classes ["result-tuple"]]
        (Vector.fromList $ zipWith tupleChild [0 ..] es)
  where
    w :: Text -> Text -> Widget (ComponentAction Search)
    w label tooltip = resultLabel rowN label (tooltipPrefix <> tooltip)
    tupleChild :: Integer -> Elem -> BoxChild (ComponentAction Search)
    tupleChild i e =
      BoxChild
        { child = elemToWidget rowN (tooltipPrefix <> tupleHelp i) e
        , properties = defaultBoxChildProperties {expand = True, fill = True}
        }

resultLabel :: Int32 -> Text -> Text -> Widget (ComponentAction Search)
resultLabel rowN label tooltip =
  case trim label of
    Nothing ->
      bin Frame [classes cls] $ widget Label ([#label := label] <> labelAttrs)
    Just trimmed ->
      container
        Box
        [#spacing := 2, classes cls]
        [ BoxChild
            defaultBoxChildProperties
            (widget Label $ [#label := trimmed] <> labelAttrs)
        , BoxChild
            defaultBoxChildProperties
            (widget Button
              [ #label := "..."
              , #tooltipText := "View full value"
              , onM #clicked onViewFullClicked
              ])
        ]
  where
    labelAttrs =
      [ #tooltipText := tooltip
      , #selectable := True
      , #singleLineMode := True
      , #halign := AlignStart
      ]
    cls
      | even rowN = ["result-cell", "result-cell-stripe"]
      | otherwise = ["result-cell"]
    onViewFullClicked _button = do
      viewFullTime <- getCurrentTime
      pure
        $ SetSearchResultsViewFull
        $ Just SearchResultsViewFull { viewFullTime, viewFullText = label }

statusbar :: Operation SearchResults -> Widget (ComponentAction Search)
statusbar res = widget Label [#label := label, #halign := AlignStart]
  where
    label
      | OperationSuccess SearchResults {searchDuration, searchSeq = rows} <- res =
        let nRows = show $ S.length rows
            dur = printf "%.3fs" (realToFrac searchDuration :: Double)
         in T.pack $ "Fetched " <> nRows <> " keys in " <> dur
      | otherwise = ""

tupleHelp :: Integer -> Text
tupleHelp index = "tuple item #" <> T.pack (show index) <> " -> "

trim :: Text -> Maybe Text
trim t
  | trimmed == t = Nothing
  | otherwise = Just trimmed
  where
    trimmed = T.replace "\n" " " $ T.replace "\r" " " $ trim' t
    trim' s
      | T.length s <= 50 = s
      | otherwise = T.take 47 s

maxKeyTupleSize :: Seq SearchResult -> Maybe Integer
maxKeyTupleSize = maxTupleSize . fmap resultKey

maxValueTupleSize :: Seq SearchResult -> Maybe Integer
maxValueTupleSize = maxTupleSize . fmap resultValue

maxTupleSize :: Seq (ByteString, Maybe [e]) -> Maybe Integer
maxTupleSize rows =
  case S.viewr $ S.sort $ fmap length . snd <$> rows of
    EmptyR -> Nothing
    _ :> a -> fromIntegral <$> a
