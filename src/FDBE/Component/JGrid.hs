{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{- Jolly-good Grid -}
module FDBE.Component.JGrid
  ( JGridCfg,
    JGridRow,
    JGridCol,
    JGridColCfg,
    colSpan,
    jgrid,
    jgrid_,
    jrow,
    jcol,
    jcol_,
  )
where

import Control.Applicative ((<|>))
import Control.Lens ((%~), (&), (.~))
import Data.Foldable (foldl')
import Data.List.Index (imap)
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import FDBE.Prelude hiding (or, span)
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Container
import Safe (maximumMay)

newtype JGridCfg = JGridCfg
  { jgcChildSpacing :: Maybe Double
  }
  deriving (Eq, Show)

instance Default JGridCfg where
  def =
    JGridCfg
      { jgcChildSpacing = Nothing
      }

instance Semigroup JGridCfg where
  c1 <> c2 =
    JGridCfg
      { jgcChildSpacing = jgcChildSpacing c2 <|> jgcChildSpacing c1
      }

instance Monoid JGridCfg where
  mempty = def

instance CmbChildSpacing JGridCfg where
  childSpacing_ spacing =
    def
      { jgcChildSpacing = Just spacing
      }

newtype JGridRow s e = JGridRow
  { jgrCols :: [JGridCol s e]
  }

data JGridCol s e = JGridCol
  { jgrContents :: WidgetNode s e,
    jgrColCfg :: JGridColCfg
  }

newtype JGridColCfg = JGridColCfg
  { jgcColSpan :: Maybe Word
  }

instance Default JGridColCfg where
  def =
    JGridColCfg
      { jgcColSpan = Nothing
      }

instance Semigroup JGridColCfg where
  c1 <> c2 =
    JGridColCfg
      { jgcColSpan = jgcColSpan c2 <|> jgcColSpan c1
      }

instance Monoid JGridColCfg where
  mempty = def

colSpan :: Word -> JGridColCfg
colSpan x = def {jgcColSpan = Just x}

newtype JGridModel = JGridModel (Seq JGridModelWidget) -- use Seq to match Monomer APIs

data JGridModelWidget = JGridModelWidget
  { jgmCol :: Int,
    jgmRow :: Int,
    jgmColSpan :: Int
  }
  deriving (Eq, Show)

jrow :: [JGridCol s e] -> JGridRow s e
jrow = JGridRow

-- todo: rename to jcell? (since it represents a single cell, not an entire column)
jcol :: WidgetNode s e -> JGridCol s e
jcol = jcol_ []

jcol_ :: [JGridColCfg] -> WidgetNode s e -> JGridCol s e
jcol_ configs widget =
  JGridCol
    { jgrContents = widget,
      jgrColCfg = mconcat configs
    }

jgrid :: forall s e. [JGridRow s e] -> WidgetNode s e
jgrid = jgrid_ def

-- todo: deal with invisible children!
-- todo: do we need to care about child padding/border?
jgrid_ :: [JGridCfg] -> [JGridRow s e] -> WidgetNode s e
jgrid_ configs unfilteredRows =
  defaultWidgetNode "FDBE.JGrid" container
    & L.children .~ S.fromList (jgrContents <$> mconcat (jgrCols <$> rows))
  where
    container =
      createContainer
        model
        def
          { containerGetSizeReq = getSizeReq,
            containerResize = resize
          }

    config = mconcat configs
    rows = filter (not . null . jgrCols) unfilteredRows
    model = rowsToModel rows
    JGridModel modelSeq = model
    nRows = length rows
    nCols = fromMaybe 0 $ maximumMay (length . jgrCols <$> rows)

    spacing = fromMaybe 0 (jgcChildSpacing config)
    spacingTotalW = spacing * max 0 (fromIntegral nCols - 1)
    spacingTotalH = spacing * max 0 (fromIntegral nRows - 1)

    getSizeReq _wenv _node children = (w, h)
      where
        (wReqs, hReqs) = toSizeReqs model children
        w = foldl' sizeReqMergeSum (fixedSize 0) wReqs & L.fixed %~ (+ spacingTotalW)
        h = foldl' sizeReqMergeSum (fixedSize 0) hReqs & L.fixed %~ (+ spacingTotalH)

    resize wenv node viewport children = (resultNode node, assignedAreas)
      where
        style = currentStyle wenv node
        Rect l t w h = fromMaybe def (removeOuterBounds style viewport)

        (wReqs, hReqs) = toSizeReqs model children
        colXs = sizesToPositions (cellSize wReqs (w - spacingTotalW))
        colYs = sizesToPositions (cellSize hReqs (h - spacingTotalH))

        assignedAreas = assignArea <$> modelSeq
        assignArea JGridModelWidget {jgmCol, jgmRow, jgmColSpan} = Rect chX chY chW chH
          where
            chX = l + S.index colXs jgmCol + spacing * fromIntegral jgmCol
            chY = t + S.index colYs jgmRow + spacing * fromIntegral jgmRow
            chW = S.index colXs (jgmCol + jgmColSpan) - S.index colXs jgmCol
            chH = S.index colYs (jgmRow + 1) - S.index colYs jgmRow

cellSize :: Seq SizeReq -> Double -> Seq Double
cellSize reqs available = reqResult <$> reqs
  where
    totalFixed = sum $ _szrFixed <$> reqs
    totalFlex = sum $ _szrFlex <$> reqs
    totalWeightedFlex = sum $ (\r -> _szrFlex r * _szrFactor r) <$> reqs
    totalWeightedExtra = sum $ (\r -> _szrExtra r * _szrFactor r) <$> reqs

    availableFlex = max 0 $ min (available - totalFixed) totalFlex
    availableExtra = available - totalFixed - availableFlex

    reqResult r
      | availableFlex >= totalFlex =
          if availableExtra > 0 && totalWeightedExtra > 0
            then
              let extraProp = _szrExtra r * _szrFactor r / totalWeightedExtra
               in _szrFixed r + _szrFlex r + availableExtra * extraProp
            else _szrFixed r + _szrFlex r
      | totalWeightedFlex > 0 =
          let flexProp = _szrFlex r * _szrFactor r / totalWeightedFlex
           in _szrFixed r + availableFlex * flexProp
      | otherwise =
          _szrFixed r

sizesToPositions :: Seq Double -> Seq Double
sizesToPositions = S.scanl (+) 0

rowsToModel :: [JGridRow s e] -> JGridModel
rowsToModel rows = JGridModel (S.fromList $ mconcat (imap mapRow rows))
  where
    mapRow y (JGridRow row) =
      snd $ foldl' (foldCol y) (0, mempty) row
    foldCol y (w, cols) (JGridCol _wgt cfg) =
      let cSpan = fromIntegral $ fromMaybe 1 (jgcColSpan cfg)
       in (w + cSpan, cols `snoc` JGridModelWidget w y cSpan)

toSizeReqs ::
  JGridModel ->
  -- | The requested sizes for each column and each row
  Seq (WidgetNode s e) ->
  (Seq SizeReq, Seq SizeReq)
toSizeReqs (JGridModel modelWidgets) widgets =
  (toSizeReqs' (sortOnSpan widgetWidths), toSizeReqs' widgetHeights)
  where
    widgetWidths =
      S.zipWith (\w m -> (_wniSizeReqW . _wnInfo $ w, jgmCol m, jgmColSpan m)) widgets modelWidgets

    widgetHeights =
      S.zipWith (\w m -> (_wniSizeReqH . _wnInfo $ w, jgmRow m, 1)) widgets modelWidgets

    sortOnSpan =
      S.sortOn (\(_, _, span) -> span)

-- todo: make less hacky/more efficient
toSizeReqs' ::
  -- | The requested size of each widget, along with its (col/row) position and (col/row) span, sorted by span asc
  Seq (SizeReq, Int, Int) ->
  -- | The computed size requests for each col/row
  Seq SizeReq
toSizeReqs' = foldl' mergeWidget mempty
  where
    mergeWidget :: Seq SizeReq -> (SizeReq, Int, Int) -> Seq SizeReq
    mergeWidget reqs (req, start, span) =
      case (occupiedReqs, unoccuppiedReqs) of
        ([], []) ->
          error $ "This is a bug! Maybe span is 0: " <> show span
        ([], ur) ->
          -- all spaces are unoccupied: split the widget equally across all spaces
          let splitReq = multSizeReq req (1 / fromIntegral span)
           in foldl'
                (\reqs' ur' -> mergeAt reqs' ur' splitReq)
                reqs
                ur
        (or, []) ->
          -- all spaces are occupied: distrbute the fixed+flex+extra size from the left,
          -- and dump any remaining in the right-most space (mostly ignore factor for now)
          let cutReq =
                foldl'
                  (\req' (_p, or') -> req' `minusSizeReq` or')
                  req
                  or
           in S.adjust (sizeReqMergeSum cutReq) (fst (last or)) reqs
        (_or, _ur) ->
          error "Todo: implement this branch! (it has not been needed yet...)"
      where
        -- fill up occupiedReqs, then the unoccupied ones with anything left over...

        occupiedReqs :: [(Int, SizeReq)]
        occupiedReqs =
          [(p, r) | p <- [start .. start + span - 1], Just r <- [S.lookup p reqs]]

        unoccuppiedReqs :: [Int]
        unoccuppiedReqs =
          [p | p <- [start .. start + span - 1], Nothing <- [S.lookup p reqs]]

multSizeReq :: SizeReq -> Double -> SizeReq
multSizeReq (SizeReq fixed flex extra factor) m =
  SizeReq
    { _szrFixed = m * fixed,
      _szrFlex = m * flex,
      _szrExtra = m * extra,
      _szrFactor = factor
    }

minusSizeReq :: SizeReq -> SizeReq -> SizeReq
minusSizeReq r1 r2 =
  SizeReq
    { _szrFixed = max 0 (_szrFixed r1 - _szrFixed r2),
      _szrFlex = max 0 (_szrFlex r1 - _szrFlex r2),
      _szrExtra = max 0 (_szrExtra r1 - _szrExtra r2),
      _szrFactor = _szrFactor r1
    }

mergeAt :: Seq SizeReq -> Int -> SizeReq -> Seq SizeReq
mergeAt reqs i req
  | S.length reqs == i = reqs |> req
  | S.length reqs > i = S.adjust' (sizeReqMergeMax req) i reqs
  | otherwise = error "There is a bug in the JGrid code: this should not happen!"
