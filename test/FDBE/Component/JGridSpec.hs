module FDBE.Component.JGridSpec (spec) where

import Control.Lens ((&), (^.), (.~))
import FDBE.Component.JGrid
import Monomer
import Monomer.TestUtil
import Test.Hspec (Spec, it, shouldBe)

import qualified Monomer.Lens as L
import qualified Data.Foldable as Foldable

spec :: Spec
spec = do
  getSizeReq
  resize

getSizeReq :: Spec
getSizeReq = do
  it "grid with no children should be empty" $ do
    size [] `shouldBe` (fixedSize 0, fixedSize 0)

  it "grid with single empty row should be empty" $ do
    size [jrow []] `shouldBe` (fixedSize 0, fixedSize 0)

  it "grid with single child should have have same size req as child" $ do
    let rw = SizeReq 1 2 3 4
        rh = SizeReq 5 6 7 8
    size [jrow [jcol $ sizedBox rw rh]] `shouldBe` (rw, rh)

  it "grid with single row should merge width size reqs" $ do
    size [jrow [
        jcol $ unitHeightBox (SizeReq 1 2 3 2),
        jcol $ unitHeightBox (SizeReq 4 5 6 1)
      ]] `shouldBe` (SizeReq 5 7 9 2, fixedSize 1)

  it "grid with single column should merge height size reqs" $ do
    size [
        jrow [jcol $ unitWidthBox (SizeReq 1 2 3 2)],
        jrow [jcol $ unitWidthBox (SizeReq 4 5 6 1)]
      ] `shouldBe` (fixedSize 1, SizeReq 5 7 9 2)

  it "grid with rows and columns should merge width and height size reqs" $ do
    size [
        jrow [
          jcol $ sizedBox (SizeReq 1 2 3 4) (SizeReq 4 3 2 1),
          jcol $ sizedBox (SizeReq 4 5 2 1) (SizeReq 8 3 4 2)
        ],
        jrow [
          jcol $ sizedBox (SizeReq 9 3 4 3) (SizeReq 4 6 2 8),
          jcol $ sizedBox (SizeReq 7 8 2 3) (SizeReq 1 2 5 7)
        ]
      ] `shouldBe` (SizeReq 16 11 6 4, SizeReq 12 9 9 8)

  it "missing trailing columns should be treated as zero size" $ do
    size [
        jrow [
          jcol $ sizedBox (SizeReq 1 2 3 4) (SizeReq 4 3 2 1),
          jcol $ sizedBox (SizeReq 4 5 2 1) (SizeReq 8 3 4 2)
        ],
        jrow [
          jcol $ sizedBox (SizeReq 9 3 4 3) (SizeReq 4 6 2 8)
        ]
      ] `shouldBe` (SizeReq 13 8 6 4, SizeReq 12 9 6 8)

  it "child spacing should be included in the size" $ do
    size_ [childSpacing_ 3] [ 
        jrow [jcol unitBox, jcol unitBox, jcol unitBox],
        jrow [jcol unitBox, jcol unitBox, jcol unitBox]
      ] `shouldBe` (fixedSize 9, fixedSize 5)

  it "child spacing should not apply to empty row" $ do
    size_ [childSpacing_ 3] [
        jrow [jcol unitBox],
        jrow [],
        jrow [jcol unitBox]
      ] `shouldBe` (fixedSize 1, fixedSize 5)

resize :: Spec
resize = do
  it "single child should receive all available space" $ do
    childViewports [
        jrow [jcol $ sizedBox (SizeReq 1 100 0 1) (SizeReq 1 100 0 1)]
      ] `shouldBe` [Rect 0 0 100 100]

  it "single row should share flex space proportionally" $ do
    childViewports [
        jrow [
          jcol $ sizedBox (SizeReq 1 100 0 1) (fixedSize 1),
          jcol $ sizedBox (SizeReq 1 100 0 2) (fixedSize 1)
        ]
      ] `shouldBe` [Rect 0 0 34 1, Rect 34 0 66 1]

  it "single row should share extra space proportionally" $ do
    childViewports [
        jrow [
          jcol $ sizedBox (SizeReq 1 0 100 1) (fixedSize 1),
          jcol $ sizedBox (SizeReq 1 0 100 2) (fixedSize 1)
        ]
      ] `shouldBe` [Rect 0 0 34 1, Rect 34 0 66 1]
  
  it "single column should share flex space proportionally" $ do
    childViewports [
        jrow [jcol $ sizedBox (fixedSize 1) (SizeReq 1 100 0 1)],
        jrow [jcol $ sizedBox (fixedSize 1) (SizeReq 1 100 0 2)]
      ] `shouldBe` [Rect 0 0 1 34, Rect 0 34 1 66]
  
  it "expanding width in a column should affect all widgets in the column" $ do
    childViewports [
        jrow [
          jcol unitBox,
          jcol unitBox
        ],
        jrow [
          jcol unitBox,
          jcol $ sizedBox (SizeReq 1 100 0 1) (fixedSize 1)
        ]
      ] `shouldBe` [
        Rect 0 0 1 1, Rect 1 0 99 1,
        Rect 0 1 1 1, Rect 1 1 99 1
      ]
  
  it "expanding height in a row should affect all widgets in the row" $ do
    childViewports [
        jrow [
          jcol unitBox,
          jcol unitBox
        ],
        jrow [
          jcol unitBox,
          jcol $ sizedBox (fixedSize 1) (SizeReq 1 100 0 1)
        ]
      ] `shouldBe` [
        Rect 0 0 1 1, Rect 1 0 1 1,
        Rect 0 1 1 99, Rect 1 1 1 99
      ]
  
  it "children should be separated by childSpacing" $ do
    childViewports_ [childSpacing_ 2] [
        jrow [
          jcol expandingBox,
          jcol expandingBox
        ],
        jrow [
          jcol expandingBox,
          jcol expandingBox
        ]
      ] `shouldBe` [
        Rect 0 0 49 49, Rect 51 0 49 49,
        Rect 0 51 49 49, Rect 51 51 49 49
      ]
  
  it "column with span=2 should expand across 2 columns" $ do
    childViewports [
        jrow [
          jcol expandingBox,
          jcol expandingBox
        ],
        jrow [
          jcol_ [colSpan 2] expandingBox
        ]
      ] `shouldBe` [
        Rect 0 0 50 50, Rect 50 0 50 50,
        Rect 0 50 100 50
      ]

windowSize :: Size
windowSize = Size 100 100

wenv :: WidgetEnv () ()
wenv = mockWenv () & L.windowSize .~ windowSize

unitWidthBox :: SizeReq -> WidgetNode () ()
unitWidthBox =
  sizedBox (fixedSize 1)

unitHeightBox :: SizeReq -> WidgetNode () ()
unitHeightBox rw =
  sizedBox rw (fixedSize 1)

unitBox :: WidgetNode () ()
unitBox =
  sizedBox (fixedSize 1) (fixedSize 1)

expandingBox :: WidgetNode () ()
expandingBox =
  sizedBox (SizeReq 1 1000 0 1) (SizeReq 1 1000 0 1)

sizedBox :: SizeReq -> SizeReq -> WidgetNode () ()
sizedBox rw rh =
  box_ [sizeReqUpdater (const (rw, rh))] (label "")

size :: [JGridRow () ()] -> (SizeReq, SizeReq)
size =
  size_ []

size_ :: [JGridCfg] -> [JGridRow () ()] -> (SizeReq, SizeReq)
size_ cfg rows =
  nodeGetSizeReq wenv (jgrid_ cfg rows)

childViewports :: [JGridRow () ()] -> [Rect]
childViewports =
  childViewports_ []

childViewports_ :: [JGridCfg] -> [JGridRow () ()] -> [Rect]
childViewports_ cfg rows = Foldable.toList childVps where
  node = nodeInit wenv (jgrid_ cfg rows)
  childVps = roundRectUnits . _wniViewport . _wnInfo <$> node ^. L.children
