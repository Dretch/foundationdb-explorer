{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{- Jolly-good Grid -}

module FDBE.Component.JGrid (
  JGridCfg(..),
  cellMarginX,
  cellMarginY,
  cellMargin,
  jgrid,
  jgrid_,
  jrow,
  jcol
) where

import FDBE.Prelude

import Control.Lens ((&), (.~))
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Container
import Data.Foldable (toList, foldl')
import Data.List ( transpose )
import qualified Data.Sequence as S
import Data.List.Split (splitPlaces)
import Control.Monad (join)
import Control.Applicative ((<|>))

data JGridCfg = JGridCfg
  { jgcCellMarginX :: Maybe Double
  , jgcCellMarginY :: Maybe Double
  }

instance Default JGridCfg where
  def = JGridCfg
    { jgcCellMarginX = Nothing
    , jgcCellMarginY = Nothing
    }

instance Semigroup JGridCfg where
  c1 <> c2 = JGridCfg
    { jgcCellMarginX = jgcCellMarginX c2 <|> jgcCellMarginX c1
    , jgcCellMarginY = jgcCellMarginY c2 <|> jgcCellMarginY c1
    }

instance Monoid JGridCfg where
  mempty = def

-- todo: make class?
cellMarginX :: Double -> JGridCfg
cellMarginX x = def { jgcCellMarginX = Just x }

-- todo: make class?
cellMarginY :: Double -> JGridCfg
cellMarginY y = def { jgcCellMarginY = Just y }

cellMargin :: Double -> JGridCfg
cellMargin xy = def { jgcCellMarginX = Just xy, jgcCellMarginY = Just xy }

newtype JGridRow s e = JGridRow
  { jgrCols :: [JGridCol s e]
  }

newtype JGridCol s e = JGridCol
  { jgrContents :: WidgetNode s e
  }

newtype JGridModel = JGridModel [[JGridColSize]]

data JGridColSize = JGridColSize -- in future this might contain colspan/rowspan/etc

jrow :: [JGridCol s e] -> JGridRow s e
jrow = JGridRow

jcol :: WidgetNode s e -> JGridCol s e
jcol = JGridCol

jgrid :: forall s e. [JGridRow s e] -> WidgetNode s e
jgrid = jgrid_ def

-- todo: deal with invisible children!
-- todo: do we need to care about child padding/border?
jgrid_ :: forall s e. [JGridCfg] -> [JGridRow s e] -> WidgetNode s e
jgrid_ configs rows =
  defaultWidgetNode "FDBE.JGrid" container
    & L.children .~ S.fromList (jgrContents <$> mconcat (jgrCols <$> rows))
  where
    config = mconcat configs
    marginX = fromMaybe 0 (jgcCellMarginX config)
    marginY = fromMaybe 0 (jgcCellMarginY config)

    container = createContainer model def {
      containerGetSizeReq = getSizeReq,
      containerResize = resize
    }

    model = JGridModel ((JGridColSize <$) . jgrCols <$> rows)
    nRows = length rows
    nCols = maximum (length . jgrCols <$> rows) -- todo: empty rows?

    -- todo: use flex/extra/factor?
    getSizeReq _wenv _node children = (w, h) where
      w = SizeReq (sum (_szrFixed <$> sizeReqPerCol children) + marginXTotal) 0 0 1 -- todo: if cols empty?
      h = SizeReq (sum (_szrFixed <$> sizeReqPerRow children) + marginYTotal) 0 0 1 -- todo: if rows empty?
      marginXTotal = max 0 (fromIntegral (nCols - 1)) * marginX
      marginYTotal = max 0 (fromIntegral (nRows - 1)) * marginY

    sizeReqPerCol :: Seq (WidgetNode s e) -> Seq SizeReq
    sizeReqPerCol children =
      sizeReqInCol <$> childrenToCols children model

    sizeReqInCol :: Seq (WidgetNode s e) -> SizeReq
    sizeReqInCol cells =
      foldl' sizeReqMergeMax (fixedSize 0) $ _wniSizeReqW . _wnInfo <$> cells  -- todo: what if rows is empty?

    sizeReqPerRow :: Seq (WidgetNode s e) -> Seq SizeReq
    sizeReqPerRow children =
      sizeReqInRow <$> childrenToRows children model

    sizeReqInRow :: Seq (WidgetNode s e) -> SizeReq
    sizeReqInRow cols =
      foldl' sizeReqMergeMax (fixedSize 0) $ _wniSizeReqH . _wnInfo <$> cols -- todo: what if rows is empty?

    resize wenv node viewport children = (resultNode node, assignedAreas) where
      style = currentStyle wenv node
      Rect l t w h = fromMaybe def (removeOuterBounds style viewport)

      assignedAreas = join $
        flip S.mapWithIndex (childrenToRows children model) $ \y row ->
          flip S.mapWithIndex row $ \x _widget ->
            let chX = l + S.index colXs x + marginX * fromIntegral x
                chY = t + S.index colYs y + marginY * fromIntegral y
                chW = S.index colXs (x + 1) - S.index colXs x
                chH = S.index colYs (y + 1) - S.index colYs y
            in Rect chX chY chW chH
      
      availableW = w - marginX * (fromIntegral nCols - 1) -- todo: empty cols?
      colXs = sizesToPositions (cellSize (sizeReqPerCol children) availableW)

      availableH = h - marginY * (fromIntegral nRows - 1)
      colYs = sizesToPositions (cellSize (sizeReqPerRow children) availableH)

-- todo: is this necessary? will children change?
childrenToRows :: Seq (WidgetNode s e) -> JGridModel -> Seq (Seq (WidgetNode s e))
childrenToRows nodes model =
  listListToSeqSeq $ childrenToRows' (toList nodes) model

-- todo: fewer Seq <-> List conversions!
childrenToRows' :: [WidgetNode s e] -> JGridModel -> [[WidgetNode s e]]
childrenToRows' nodes (JGridModel cs) =
  splitPlaces (length <$> cs) nodes

childrenToCols :: Seq (WidgetNode s e) -> JGridModel -> Seq (Seq (WidgetNode s e))
childrenToCols nodes model =
  -- todo: check behaviour with irregularly sized rows is sensible
  listListToSeqSeq $ transpose $ childrenToRows' (toList nodes) model

listListToSeqSeq :: [[x]] -> Seq (Seq x)
listListToSeqSeq = S.fromList . (S.fromList <$>)

-- todo: investigate weird behaviour (when not enough space for all flex?)
cellSize :: Seq SizeReq -> Double -> Seq Double
cellSize reqs available = reqResult <$> reqs where

  totalFixed = sum $ _szrFixed <$> reqs
  totalFlex = sum $ _szrFlex <$> reqs
  totalFactor = sum $ _szrFactor <$> reqs
  totalWeightedExtra = sum $ (\r -> _szrExtra r * _szrFactor r) <$> reqs

  availableFlex = min (available - totalFixed) totalFlex
  availableExtra = available - totalFixed - availableFlex

  reqResult r
    | availableFlex > 0 && availableFlex >= totalFlex =
      if availableExtra > 0 && totalWeightedExtra > 0 then
        _szrFixed r + _szrFlex r + (_szrExtra r * _szrFactor r / totalWeightedExtra) * availableExtra
      else
        _szrFixed r + _szrFlex r
    | availableFlex > 0 && _szrFactor r > 0 =
      _szrFixed r + availableFlex * (_szrFactor r / totalFactor)
    | otherwise =
      _szrFixed r

sizesToPositions :: Seq Double -> Seq Double
sizesToPositions = S.scanl (+) 0