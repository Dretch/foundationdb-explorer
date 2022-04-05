{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module FDBE.Component.Search (search) where

import Control.Exception (displayException)
import Control.Lens
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import qualified Data.UUID as UUID
import FDBE.Bytes (bytesToText)
import FDBE.Component.JGrid (JGridCell, JGridRow, colSpan, jcell, jcell_, jgrid, jgrid_, jrow)
import FDBE.Component.TupleEntry
import qualified FDBE.Font as Font
import FDBE.FoundationDB (SearchRange (..), SearchResult (..), getSearchResult, searchFrom, searchLimit, searchReverse, searchTo)
import FDBE.Monomer (useOldCompositeModel)
import FDBE.Prelude
import FDBE.State (Operation (..))
import FoundationDB (Database)
import FoundationDB.Layer.Tuple (Elem)
import qualified FoundationDB.Layer.Tuple as LT
import FoundationDB.Versionstamp (TransactionVersionstamp (TransactionVersionstamp), Versionstamp (CompleteVersionstamp, IncompleteVersionstamp))
import Monomer hiding (width)

data SearchModel = SearchModel
  { _smDatabase :: Database,
    _smRange :: SearchRange,
    _smResults :: Operation SearchResults
  }
  deriving (Eq, Show)

data SearchResults = SearchResults
  { _srDuration :: NominalDiffTime,
    _srSeq :: Seq SearchResult
  }
  deriving (Eq, Show)

type ShowEditorEvent e = ByteString -> ByteString -> e

makeLensesWith abbreviatedFields ''SearchModel

data SearchEvent
  = StartSearch
  | FinishSearch (Either Text (NominalDiffTime, Seq SearchResult))
  | EditSearchResult ByteString ByteString

search ::
  (Typeable s, Typeable e) =>
  Database ->
  ShowEditorEvent e ->
  WidgetNode s e
search db showEditorEvent = comp
  where
    comp =
      compositeD_
        "FBBE.Search"
        (WidgetValue initialModel)
        buildUI
        (handleEvent showEditorEvent)
        [useOldCompositeModel]
    initialModel =
      SearchModel
        { _smDatabase = db,
          _smRange =
            SearchRange
              { _searchFrom = Left "",
                _searchTo = Left "\\xFF",
                _searchLimit = 100,
                _searchReverse = False
              },
          _smResults = OperationNotStarted
        }

buildUI :: UIBuilder SearchModel SearchEvent
buildUI _wenv model = searchGrid
  where
    searchGrid =
      jgrid_
        [childSpacing_ 2]
        [ jrow
            [ jcell $ label "From",
              jcell $ tupleEntry (range . searchFrom) -- todo: allow selecting "Start"/"End" tuples?
            ],
          jrow
            [ jcell $ label "To",
              jcell $ tupleEntry (range . searchTo)
            ],
          jrow
            [ jcell $ label "Limit",
              jcell $ numericField_ (range . searchLimit) [minValue 0, wheelRate 10] -- todo: why is minValue ignored?
            ],
          jrow
            [ jcell spacer,
              jcell $ labeledCheckbox_ "Reverse Order" (range . searchReverse) [textRight, childSpacing_ 2]
            ],
          jrow
            [ jcell_ [colSpan 2] $
                hstack
                  [ filler,
                    mainButton "Fetch" StartSearch
                  ]
            ],
          jrow
            [ jcell_ [colSpan 2] resultsGrid
            ],
          jrow
            [ jcell_ [colSpan 2] $
                vstack
                  [ filler,
                    label statusText `styleBasic` [paddingV 2]
                  ]
            ]
        ]
        `styleBasic` [padding 2]
        `nodeEnabled` searchNotInProgress

    resultsGrid = case model ^. results of
      OperationNotStarted ->
        label ""
      OperationInProgress ->
        label "Loading..." `styleBasic` [textCenter]
      OperationFailure msg ->
        label ("Search Failed: " <> msg) `styleBasic` [textCenter]
      OperationSuccess SearchResults {_srSeq = rows} ->
        let keyWidth = fromMaybe 1 $ maxKeyTupleSize rows
            valueWidth = fromMaybe 1 $ maxValueTupleSize rows
         in scroll $
              jgrid $
                zipWith (resultRow keyWidth valueWidth) rowBgs (Foldable.toList rows)

    statusText = case model ^. results of
      OperationSuccess SearchResults {_srDuration, _srSeq} ->
        let nRows = show $ S.length _srSeq
            dur = printf "%.3fs" (realToFrac _srDuration :: Double)
         in T.pack $ "Fetched " <> nRows <> " keys in " <> dur
      _ ->
        ""

    searchNotInProgress =
      model ^. results /= OperationInProgress

resultRow :: Word -> Word -> Color -> SearchResult -> JGridRow SearchModel SearchEvent
resultRow keyWidth valueWidth bgCol SearchResult {_resultKey, _resultValue} =
  jrow $
    [editCell] <> keyCells <> [eqCell] <> valueCells
  where
    editCell :: JGridCell s SearchEvent
    editCell =
      jcell $
        button "Edit" (EditSearchResult (fst _resultKey) (fst _resultValue))
          `styleBasic` [textSize 10, border 0 white]

    eqCell :: JGridCell s e
    eqCell =
      jcell $
        label "=" `styleBasic` [textFont Font.monoBold, padding 4]

    keyCells :: [JGridCell s e]
    keyCells
      | (t, Nothing) <- _resultKey =
          [rawCell keyWidth t]
      | (_, Just ts) <- _resultKey =
          imap (elemCell . tupleHelp) ts <> spacerCells (fromIntegral keyWidth - length ts)

    valueCells :: [JGridCell s e]
    valueCells
      | (t, Nothing) <- _resultValue =
          [rawCell valueWidth t]
      | (_, Just ts) <- _resultValue =
          imap (elemCell . tupleHelp) ts

    rawCell :: Word -> ByteString -> JGridCell s e
    rawCell width bytes =
      jcell_ [colSpan width] $
        tooltip "Raw bytes (not a tuple)" $
          labelSS $ label (bytesToText bytes)

    elemCell :: Text -> Elem -> JGridCell s e
    elemCell tooltipPrefix elm =
      jcell $ elemToWidget labelSS tooltipPrefix elm

    spacerCells :: Int -> [JGridCell s e]
    spacerCells n =
      L.replicate n (jcell spacer)

    labelSS :: LabelStyleSetter
    labelSS w = w `styleBasic` [bgColor bgCol, padding 6]

rowBgs :: [Color]
rowBgs = rowBgDark : rowBgLight : rowBgs

handleEvent :: ShowEditorEvent ep -> EventHandler SearchModel SearchEvent sp ep
handleEvent showEditorEvent _wenv _node model = \case
  StartSearch ->
    [ Model (storing results OperationInProgress model),
      Task $ do
        res <- getSearchResult (model ^. database) (model ^. range)
        pure . FinishSearch $ mapLeft (T.pack . displayException) res
    ]
  FinishSearch newResults ->
    let mkSuccess (searchDuration', searchSeq') =
          OperationSuccess (SearchResults searchDuration' searchSeq')
        searchResults' = either OperationFailure mkSuccess newResults
     in [Model (model & results .~ searchResults')]
  EditSearchResult key value ->
    [Report (showEditorEvent key value)]

type LabelStyleSetter = forall s e. WidgetNode s e -> WidgetNode s e

elemToWidget :: LabelStyleSetter -> Text -> Elem -> WidgetNode s e
elemToWidget labelSS tooltipPrefix =
  \case
    LT.None -> w "null" "null value"
    LT.Bytes bs -> w (trimWithEllipsis (bytesToText bs)) "binary data"
    LT.Text t -> w (trimWithEllipsis t) "text"
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
      hstack $
        flip imap es $ \i e ->
          elemToWidget labelSS (tooltipPrefix <> tupleHelp i) e
            `styleBasic` [border 1 rowTupleBorder, paddingH 6, paddingV 2]
  where
    w :: Text -> Text -> WidgetNode s e
    w text tooltipText =
      tooltip (tooltipPrefix <> tooltipText) $
        labelSS $ label text

    trimWithEllipsis :: Text -> Text
    trimWithEllipsis t =
      if T.length t > 100
        then T.take 99 t <> "\x2026"
        else t

tupleHelp :: Int -> Text
tupleHelp i = "tuple item #" <> T.pack (show i) <> " -> "

maxKeyTupleSize :: Seq SearchResult -> Maybe Word
maxKeyTupleSize = maxTupleSize . fmap _resultKey

maxValueTupleSize :: Seq SearchResult -> Maybe Word
maxValueTupleSize = maxTupleSize . fmap _resultValue

maxTupleSize :: Seq (a, Maybe [b]) -> Maybe Word
maxTupleSize rows =
  case S.viewr $ S.sort $ fmap length . snd <$> rows of
    EmptyR -> Nothing
    _ S.:> a -> fromIntegral <$> a

rowBgLight :: Color
rowBgLight = rgbHex "#fcfcfc"

rowBgDark :: Color
rowBgDark = rgbHex "#f3f6f6"

rowTupleBorder :: Color
rowTupleBorder = rgbHex "#e5e5e5"
