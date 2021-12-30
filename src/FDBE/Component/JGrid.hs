{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{- Jolly-good Grid -}

module FDBE.Component.JGrid (
  JGridCfg,
  JGridRow,
  JGridCol,
  JGridColCfg,
  cellMarginX,
  cellMarginY,
  cellMargin,
  colSpan,
  rowSpan,
  jgrid,
  jgrid_,
  jrow,
  jcol,
  jcol_
) where

import FDBE.Prelude

import Control.Lens ((&), (.~))
import Monomer
import qualified Monomer.Lens as L
import Monomer.Widgets.Container
import Data.Foldable (toList, foldl')
import Data.List.Index (imap)
import qualified Data.Sequence as S
import Control.Applicative ((<|>))
import Data.Sequence ((|>))

data JGridCfg = JGridCfg
  { jgcCellMarginX :: Maybe Double
  , jgcCellMarginY :: Maybe Double
  } deriving (Eq, Show)

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

data JGridCol s e = JGridCol
  { jgrContents :: WidgetNode s e
  , jgrColCfg :: JGridColCfg
  }

data JGridColCfg = JGridColCfg
  { jgcColSpan :: Maybe Word
  , jgcRowSpan :: Maybe Word
  }

instance Default JGridColCfg where
  def = JGridColCfg
    { jgcColSpan = Nothing
    , jgcRowSpan = Nothing
    }

instance Semigroup JGridColCfg where
  c1 <> c2 = JGridColCfg
    { jgcColSpan = jgcColSpan c2 <|> jgcColSpan c1
    , jgcRowSpan = jgcColSpan c2 <|> jgcColSpan c1
    }

instance Monoid JGridColCfg where
  mempty = def

-- todo: make class?
colSpan :: Word -> JGridColCfg
colSpan x = def { jgcColSpan = Just x }

-- todo: make class?
rowSpan :: Word -> JGridColCfg
rowSpan x = def { jgcRowSpan = Just x }

newtype JGridModel = JGridModel [JGridModelWidget] -- todo: use Seq?

data JGridModelWidget = JGridModelWidget -- todo: use Word?
  { jgmCol :: Int
  , jgmRow :: Int
  , jgmColSpan :: Int
  , jgmRowSpan :: Int
  } deriving (Eq, Show)

jrow :: [JGridCol s e] -> JGridRow s e
jrow = JGridRow

jcol :: WidgetNode s e -> JGridCol s e
jcol = jcol_ []

jcol_ :: [JGridColCfg] -> WidgetNode s e  -> JGridCol s e
jcol_ configs widget = JGridCol widget (mconcat configs)

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

    model = rowsToModel rows
    JGridModel modelList = model
    nRows = length rows
    nCols = maximum (length . jgrCols <$> rows) -- todo: empty rows?

    -- todo: use flex/extra/factor?
    getSizeReq _wenv _node children = (w, h) where
      (wReqs, hReqs) = toReqSizes model (toList children)
      w = SizeReq (sum (_szrFixed <$> wReqs) + marginXTotal) 0 0 1
      h = SizeReq (sum (_szrFixed <$> hReqs) + marginYTotal) 0 0 1
      marginXTotal = max 0 (fromIntegral (nCols - 1)) * marginX
      marginYTotal = max 0 (fromIntegral (nRows - 1)) * marginY

    resize wenv node viewport children = (resultNode node, assignedAreas) where
      style = currentStyle wenv node
      Rect l t w h = fromMaybe def (removeOuterBounds style viewport)

      assignedAreas =
        flip fmap (S.zip children (S.fromList modelList)) $ \(_child, childModel) ->
          let JGridModelWidget{jgmCol, jgmRow, jgmColSpan, jgmRowSpan} = childModel
              chX = l + S.index colXs jgmCol + marginX * fromIntegral jgmCol
              chY = t + S.index colYs jgmRow + marginY * fromIntegral jgmRow
              chW = S.index colXs (jgmCol + jgmColSpan) - S.index colXs jgmCol
              chH = S.index colYs (jgmRow + jgmRowSpan) - S.index colYs jgmRow
          in Rect chX chY chW chH

      (wReqs', hReqs') = toReqSizes model (toList children)
      wReqs = trace ("wReqs=" <> show wReqs') wReqs'
      hReqs = trace ("hReqs=" <> show hReqs') hReqs'

      availableW = w - marginX * (fromIntegral nCols - 1) -- todo: empty cols?
      colXs = sizesToPositions (cellSize wReqs availableW)

      availableH = h - marginY * (fromIntegral nRows - 1)
      colYs = sizesToPositions (cellSize hReqs availableH)

cellSize :: Seq SizeReq -> Double -> Seq Double
cellSize reqs available = reqResult <$> reqs where

  totalFixed = sum $ _szrFixed <$> reqs
  totalFlex = sum $ _szrFlex <$> reqs
  totalWeightedFlex = sum $ (\r -> _szrFlex r * _szrFactor r) <$> reqs
  totalWeightedExtra = sum $ (\r -> _szrExtra r * _szrFactor r) <$> reqs

  availableFlex = max 0 $ min (available - totalFixed) totalFlex
  availableExtra = available - totalFixed - availableFlex

  reqResult r
    | availableFlex > 0 && availableFlex >= totalFlex =
      if availableExtra > 0 && totalWeightedExtra > 0 then
        let extraProp = _szrExtra r * _szrFactor r / totalWeightedExtra
         in _szrFixed r + _szrFlex r + availableExtra * extraProp
      else
        _szrFixed r + _szrFlex r
    | totalWeightedFlex > 0 =
      let flexProp = _szrFlex r * _szrFactor r / totalWeightedFlex
       in _szrFixed r + availableFlex * flexProp
    | otherwise =
      _szrFixed r

sizesToPositions :: Seq Double -> Seq Double
sizesToPositions = S.scanl (+) 0

rowsToModel :: [JGridRow s e] -> JGridModel
rowsToModel rows = JGridModel (mconcat (imap mapRow rows)) where
  mapRow y (JGridRow row) =
    snd $ foldl' (foldCol y) (0, mempty) row
  foldCol y (w, cols) (JGridCol _wgt cfg) =
    let cSpan = fromIntegral $ fromMaybe 1 (jgcColSpan cfg)
     in (w + cSpan, cols `snoc` JGridModelWidget w y cSpan 1) -- todo: support rowspan!

-- todo: make less ugly / more efficient
toReqSizes
 :: JGridModel
 -> [WidgetNode s e]
 -> (Seq SizeReq, Seq SizeReq) -- ^ The requested sizes for each column and each row
toReqSizes (JGridModel modelWidgets) widgets =
  foldl'
    (\(cols, rows) (x, y, w, h) ->
      (mergeAt cols x w, mergeAt rows y h))
    (mempty, mempty)
    xySizes
  where
    xySizes :: [(Int, Int, SizeReq, SizeReq)]
    xySizes = mconcat $ flip fmap (zip modelWidgets widgets) $ \(JGridModelWidget{jgmCol, jgmRow, jgmColSpan, jgmRowSpan}, widget) ->
      let w = multSizeReq (_wniSizeReqW . _wnInfo $ widget) (1 / fromIntegral jgmColSpan)
          h = multSizeReq (_wniSizeReqH . _wnInfo $ widget) (1 / fromIntegral jgmRowSpan)
      in [(x, y, w, h) | x <- [jgmCol..jgmCol + jgmColSpan - 1]
                       , y <- [jgmRow..jgmRow + jgmRowSpan - 1]]

    mergeAt :: Seq SizeReq -> Int -> SizeReq -> Seq SizeReq
    mergeAt reqs i req
      | S.length reqs == i = reqs |> req
      | S.length reqs > i  = S.adjust' (sizeReqMergeMax req) i reqs
      | otherwise          = error "There is a bug in the JGrid code: this should not happen!"

multSizeReq :: SizeReq -> Double -> SizeReq
multSizeReq (SizeReq fixed flex extra factor) m = SizeReq {
    _szrFixed = m * fixed,
    _szrFlex = m * flex,
    _szrExtra = m * extra,
    _szrFactor = factor
  }