{- Helpers to make Monomer easier to use -}
module FDBE.Monomer
  ( sizeReqUpdaterFlexMax,
    useOldCompositeModel,
  )
where

import Control.Lens
import FDBE.Prelude
import Monomer hiding (textColor)

sizeReqUpdaterFlexMax :: CmbSizeReqUpdater t => t
sizeReqUpdaterFlexMax = sizeReqUpdater (bimap flexMax flexMax)
  where
    flexMax req = req {_szrFlex = 10000}

useOldCompositeModel :: CompositeCfg s e sp ep
useOldCompositeModel =
  compositeMergeModel $ \_wenv _parentModel oldModel _newModel -> oldModel
