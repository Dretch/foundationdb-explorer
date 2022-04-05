{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{- Jolly-good Grid -}
module FDBE.Component.JGrid
  ( JGridCfg,
    JGridRow,
    JGridCell,
    JGridCellCfg,
    colSpan,
    jgrid,
    jgrid_,
    jrow,
    jcell,
    jcell_,
  )
where

import Control.Applicative ((<|>))
import Control.Lens ((%~), (&), (.~))
import Data.Foldable (foldl')
import Data.List.Index (imap)
import Data.Sequence (Seq ((:|>)))
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
  { jgrCells :: [JGridCell s e]
  }

data JGridCell s e = JGridCell
  { jgrContents :: WidgetNode s e,
    jgrCellCfg :: JGridCellCfg
  }

newtype JGridCellCfg = JGridCellCfg
  { jgcColSpan :: Maybe Word
  }

instance Default JGridCellCfg where
  def =
    JGridCellCfg
      { jgcColSpan = Nothing
      }

instance Semigroup JGridCellCfg where
  c1 <> c2 =
    JGridCellCfg
      { jgcColSpan = jgcColSpan c2 <|> jgcColSpan c1
      }

instance Monoid JGridCellCfg where
  mempty = def

colSpan :: Word -> JGridCellCfg
colSpan x = def {jgcColSpan = Just x}

data JGridPosition = JGridPosition
  { jgmCol :: Int,
    jgmRow :: Int,
    jgmColSpan :: Int
  }
  deriving (Eq, Show)

jrow :: [JGridCell s e] -> JGridRow s e
jrow = JGridRow

jcell :: WidgetNode s e -> JGridCell s e
jcell = jcell_ []

jcell_ :: [JGridCellCfg] -> WidgetNode s e -> JGridCell s e
jcell_ configs widget =
  JGridCell
    { jgrContents = widget,
      jgrCellCfg = mconcat configs
    }

jgrid :: forall s e. [JGridRow s e] -> WidgetNode s e
jgrid = jgrid_ def

-- todo: deal with invisible children!
-- todo: do we need to care about child padding/border?
jgrid_ :: [JGridCfg] -> [JGridRow s e] -> WidgetNode s e
jgrid_ configs unfilteredRows =
  defaultWidgetNode "FDBE.JGrid" container
    & L.children .~ S.fromList (jgrContents <$> mconcat (jgrCells <$> rows))
  where
    container =
      createContainer
        ()
        def
          { containerGetSizeReq = getSizeReq,
            containerResize = resize
          }

    config = mconcat configs
    rows = filter (not . null . jgrCells) unfilteredRows
    gridPositions = rowsToGridPositions rows
    nRows = length rows
    nCols = fromMaybe 0 $ maximumMay (length . jgrCells <$> rows)

    spacing = fromMaybe 0 (jgcChildSpacing config)
    spacingTotalW = spacing * max 0 (fromIntegral nCols - 1)
    spacingTotalH = spacing * max 0 (fromIntegral nRows - 1)

    getSizeReq _wenv _node children = (w, h)
      where
        (wReqs, hReqs) = toSizeReqs gridPositions children
        w = foldl' sizeReqMergeSum (fixedSize 0) wReqs & L.fixed %~ (+ spacingTotalW)
        h = foldl' sizeReqMergeSum (fixedSize 0) hReqs & L.fixed %~ (+ spacingTotalH)

    resize wenv node viewport children = (resultNode node, assignedAreas)
      where
        style = currentStyle wenv node
        Rect l t w h = fromMaybe def (removeOuterBounds style viewport)

        (wReqs, hReqs) = toSizeReqs gridPositions children
        colXs = sizesToPositions (cellSize wReqs (w - spacingTotalW))
        rowYs = sizesToPositions (cellSize hReqs (h - spacingTotalH))

        assignedAreas = assignArea <$> gridPositions
        assignArea JGridPosition {jgmCol, jgmRow, jgmColSpan} = Rect chX chY chW chH
          where
            chX = l + S.index colXs jgmCol + spacing * fromIntegral jgmCol
            chY = t + S.index rowYs jgmRow + spacing * fromIntegral jgmRow
            chW = S.index colXs (jgmCol + jgmColSpan) - S.index colXs jgmCol
            chH = S.index rowYs (jgmRow + 1) - S.index rowYs jgmRow

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

rowsToGridPositions :: [JGridRow s e] -> Seq JGridPosition
rowsToGridPositions rows = mconcat (imap mapRow rows)
  where
    mapRow y (JGridRow row) =
      snd $ foldl' (foldCol y) (0, mempty) row
    foldCol y (w, cols) (JGridCell _wgt cfg) =
      let cSpan = fromIntegral $ fromMaybe 1 (jgcColSpan cfg)
       in (w + cSpan, cols :|> JGridPosition w y cSpan)

toSizeReqs ::
  Seq JGridPosition ->
  -- | The requested sizes for each column and each row
  Seq (WidgetNode s e) ->
  (Seq SizeReq, Seq SizeReq)
toSizeReqs modelWidgets widgets =
  (toSizeReqs' (sortOnSpan widgetWidths), toSizeReqs' widgetHeights)
  where
    widgetWidths =
      S.zipWith (\w m -> (_wniSizeReqW . _wnInfo $ w, jgmCol m, jgmColSpan m)) widgets modelWidgets

    widgetHeights =
      S.zipWith (\w m -> (_wniSizeReqH . _wnInfo $ w, jgmRow m, 1)) widgets modelWidgets

    sortOnSpan =
      S.sortOn (\(_, _, span) -> span)

    toSizeReqs' =
      foldl' mergeWidgetSizeReq mempty

mergeWidgetSizeReq :: Seq SizeReq -> (SizeReq, Int, Int) -> Seq SizeReq
mergeWidgetSizeReq reqs (req, start, span) =
  if missingReqs == 0
    then -- all cells are occupied so merge any additional space into the right-most cell
      S.adjust (sizeReqMergeSum remainingReq) (start + span - 1) reqs
    else -- some cells are unoccupied, so split any additional space evenly between them
      reqs <> S.replicate missingReqs splitRemainingReq
  where
    spanReqs = S.take span (S.drop start reqs)

    missingReqs = span - length spanReqs

    remainingReq =
      foldl' minusSizeReq req spanReqs

    splitRemainingReq =
      multSizeReq remainingReq (1 / fromIntegral missingReqs)

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
