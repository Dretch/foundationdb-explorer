{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module FDBE.Component.Search (search) where

import FDBE.Prelude
import FDBE.Component.TupleEntry
import FDBE.FoundationDB (SearchResult (..), SearchRange (..), searchFrom, searchTo, searchLimit, searchReverse, getSearchResult)

import Control.Lens
import qualified Data.Sequence                               as S
import qualified Data.Text                as T
import qualified Data.List                as L
import Monomer hiding (width)
import FoundationDB (Database)
import Data.Time (NominalDiffTime)
import FDBE.State (Operation(..))
import Control.Exception (displayException)
import qualified Data.Foldable as Foldable
import FDBE.Bytes (bytesToText)
import FDBE.Component.JGrid (JGridRow, jcol, jrow, childSpacing, jgrid_, colSpan, jcol_, JGridCol, jgrid)
import qualified FDBE.Font as Font
import FoundationDB.Layer.Tuple (Elem)
import qualified FoundationDB.Layer.Tuple as LT
import qualified Data.UUID as UUID
import FoundationDB.Versionstamp (Versionstamp(CompleteVersionstamp, IncompleteVersionstamp), TransactionVersionstamp (TransactionVersionstamp))

data SearchModel = SearchModel
  { _database :: Database
  , _searchRange :: SearchRange
  , _searchResults :: Operation SearchResults
  }
  deriving (Eq, Show)

data SearchResults =
  SearchResults
    { _searchDuration :: NominalDiffTime
    , _searchSeq      :: Seq SearchResult
    }
  deriving (Eq, Show)

-- todo: why do these have to be together? https://stackoverflow.com/questions/47742054/haskell-makelenses-data-constructor-not-in-scope
makeLenses ''SearchModel
makeLenses ''SearchResults

data SearchEvent
  = forall a. SetValue (ALens' SearchModel a) a
  | StartSearch
  | FinishSearch (Either Text (NominalDiffTime, Seq SearchResult))

buildUI ::
  WidgetEnv SearchModel SearchEvent ->
  SearchModel ->
  WidgetNode SearchModel SearchEvent
buildUI _wenv model = widgetTree where

  widgetTree =
    jgrid_ [childSpacing 2] [
      jrow [
        jcol $ label "From",
        jcol $ tupleEntry (model ^. searchRange . searchFrom) (SetValue $ searchRange . searchFrom)
      ],
      jrow [
        jcol $ label "To",
        jcol $ tupleEntry (model ^. searchRange . searchTo) (SetValue $ searchRange . searchTo)
      ],
      jrow [
        jcol $ label "Limit",
        jcol $ numericField_ (searchRange . searchLimit) [minValue 0, wheelRate 10] -- todo: why is minValue ignored?
      ],
      jrow [
        jcol spacer,
        jcol $ labeledCheckbox_ "Reverse Order" (searchRange . searchReverse) [textRight]
      ],
      jrow [
        jcol_ [colSpan 2] $ hstack [
          filler,
          mainButton "Fetch" StartSearch
        ]
      ],
      jrow [
        jcol_ [colSpan 2] results
      ],
      jrow [
        jcol_ [colSpan 2] $ vstack [
          filler,
          label statusText `styleBasic` [paddingV 2]
        ]
      ]
    ] `styleBasic` [padding 2]  `nodeEnabled` searchNotInProgress

  results = case model ^. searchResults of
    OperationNotStarted ->
      label ""
    OperationInProgress ->
      label "Loading..." `styleBasic` [textCenter]
    OperationFailure msg ->
      label ("Search Failed: " <> msg) `styleBasic` [textCenter]
    OperationSuccess SearchResults { _searchSeq = rows } ->
      let keyWidth = fromMaybe 1 $ maxKeyTupleSize rows
          valueWidth = fromMaybe 1 $ maxValueTupleSize rows
       in scroll $
            jgrid $
              zipWith (resultRow keyWidth valueWidth) rowBgs (Foldable.toList rows)

  statusText = case model ^. searchResults of
    OperationSuccess SearchResults { _searchDuration, _searchSeq } ->
      let nRows = show $ S.length _searchSeq
          dur = printf "%.3fs" (realToFrac _searchDuration :: Double)
       in T.pack $ "Fetched " <> nRows <> " keys in " <> dur
    _ ->
     ""
  
  searchNotInProgress =
    model ^. searchResults /= OperationInProgress

resultRow :: Word -> Word -> Color -> SearchResult -> JGridRow SearchModel SearchEvent
resultRow keyWidth valueWidth bgCol SearchResult { _resultKey, _resultValue } =
  jrow $
    keyCells <> [eqCell] <> valueCells
  where
    eqCell :: JGridCol s e
    eqCell =
      -- todo: why does this sometimes not show up?(when using reverse search...)
      jcol $
        label "=" `styleBasic` [textFont Font.monoBold, padding 4]

    keyCells :: [JGridCol s e]
    keyCells
      | (t, Nothing) <- _resultKey =
        [rawCell keyWidth t]
      | (_, Just ts) <- _resultKey =
        imap (elemCell . tupleHelp) ts <> spacerCells (fromIntegral keyWidth - length ts)
    
    valueCells :: [JGridCol s e]
    valueCells
      | (t, Nothing) <- _resultValue =
        [rawCell valueWidth t]
      | (_, Just ts) <- _resultValue =
        imap (elemCell . tupleHelp) ts
    
    rawCell :: Word -> ByteString -> JGridCol s e
    rawCell width bytes =
      jcol_ [colSpan width] $
        labelSS $ label (bytesToText bytes) -- todo: tooltip?
    
    elemCell :: Text -> Elem -> JGridCol s e
    elemCell tooltipPrefix elm =
      jcol $ elemToWidget labelSS tooltipPrefix elm
    
    spacerCells :: Int -> [JGridCol s e]
    spacerCells n =
      L.replicate n (jcol spacer)
    
    labelSS :: LabelStyleSetter
    labelSS w = w
      `styleBasic` [bgColor bgCol, padding 6]
      `styleHover` [bgColor rowBgHover, cursorIcon CursorHand]

rowBgs :: [Color]
rowBgs = rowBgDark : rowBgLight : rowBgs

handleEvent ::
  WidgetEnv SearchModel SearchEvent ->
  WidgetNode SearchModel SearchEvent ->
  SearchModel ->
  SearchEvent ->
  [EventResponse SearchModel SearchEvent ep sp]
handleEvent _wenv _node model = \case
  SetValue setter a ->
    [Model (storing setter a model)]
  StartSearch ->
    [ Model (storing searchResults OperationInProgress model)
    , Task $ do
        res <- getSearchResult (model ^. database) (model ^. searchRange)
        pure . FinishSearch $ mapLeft (T.pack . displayException) res
    ]
  FinishSearch results ->
    let mkSuccess (searchDuration', searchSeq') =
          OperationSuccess (SearchResults searchDuration' searchSeq')
        searchResults' = either OperationFailure mkSuccess results
    in [Model (model & searchResults .~ searchResults')]

search :: (Typeable s, Typeable e) => Database -> WidgetNode s e
search db = compositeD_ "FBBE.Search" (WidgetValue initialModel) buildUI handleEvent [] where
  initialModel = SearchModel
    { _database = db
    , _searchRange = SearchRange
        { _searchFrom = Left ""
        , _searchTo = Left "\\xFF"
        , _searchLimit = 100
        , _searchReverse = False
        }
    , _searchResults = OperationNotStarted
    }

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
        -- todo: why text not showing up!?
        hstack $ flip imap es $ \i e ->
          elemToWidget labelSS (tooltipPrefix <> tupleHelp i) e
            `styleBasic` [border 1 rowTupleBorder, paddingH 6, paddingV 2]
  where
    w :: Text -> Text -> WidgetNode s e
    w text tooltipText =
      tooltip (tooltipPrefix <> tooltipText) $
        labelSS $ label text

    trimWithEllipsis :: Text -> Text
    trimWithEllipsis t =
      if T.length t > 100 then
        T.take 99 t <> "\x2026"
      else
        t

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

rowBgHover :: Color
rowBgHover = rgbHex "#ffffff"
