{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module FDBE.Component.Search (search) where

import FDBE.Prelude
import FDBE.Component.TupleEntry
import FDBE.FoundationDB (SearchResult (..), SearchRange (..), searchFrom, searchTo, searchLimit, searchReverse, getSearchResult)

import Control.Lens
import qualified Data.Text                as T
import Monomer
import FoundationDB (Database)
import Data.Time (NominalDiffTime)
import FDBE.State (Operation(..))
import Control.Exception (displayException)
import qualified Data.Sequence as S
import qualified Data.Foldable as Foldable
import FDBE.Bytes (bytesToText)
import FDBE.Component.JGrid (JGridRow, jcol, jrow, cellMargin, jgrid_)
import qualified FDBE.Font as Font

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
    jgrid_ [cellMargin 4] [
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
        jcol spacer,
        jcol $ hstack [
          filler,
          button "Fetch" StartSearch
        ]
      ],
      jrow [
        jcol spacer,
        jcol results
      ],
      jrow [
        jcol spacer,
        jcol $ label statusText
      ]
    ] `styleBasic` [padding 4]  `nodeEnabled` searchNotInProgress

  results = case model ^. searchResults of
    OperationNotStarted ->
      label ""
    OperationInProgress ->
      label "Loading..." `styleBasic` [textCenter]
    OperationFailure msg ->
      label msg `styleBasic` [textCenter]
    OperationSuccess SearchResults { _searchSeq } ->
      scroll $
        jgrid_ [cellMargin 4] (Foldable.toList (resultRow <$> _searchSeq))

  statusText = case model ^. searchResults of
    OperationSuccess SearchResults { _searchDuration, _searchSeq } ->
      let nRows = show $ S.length _searchSeq
          dur = printf "%.3fs" (realToFrac _searchDuration :: Double)
       in T.pack $ "Fetched " <> nRows <> " keys in " <> dur
    _ ->
     ""
  
  searchNotInProgress =
    model ^. searchResults /= OperationInProgress

resultRow :: SearchResult -> JGridRow SearchModel SearchEvent
resultRow SearchResult { _resultKey, _resultValue } =
  jrow [
    jcol $ label (bytesToText (_resultKey ^. _1)),
    jcol $ label "=" `styleBasic` [textFont Font.monoBold], -- todo: why does this sometimes not show up?
    jcol $ label (bytesToText (_resultValue ^. _1))
  ]

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