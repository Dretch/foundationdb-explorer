module FDBE.Prelude
  ( module X
  , identity
  ) where

import           Prelude                 as X hiding (id)

import           Control.DeepSeq         as X (NFData, force)
import           Control.Error.Util      as X (hush)
import           Control.Exception       as X (evaluate, try)
import           Control.Monad           as X (forM, forM_, void, when)
import           Control.Monad.IO.Class  as X (liftIO)
import           Data.ByteString         as X (ByteString)
import           Data.Default.Class      as X (Default, def)
import           Data.Either.Combinators as X (leftToMaybe)
import           Data.Either.Extra       as X (mapLeft)
import           Data.Functor            as X ((<&>))
import           Data.Hashable           as X (Hashable)
import           Data.HashMap.Strict     as X (HashMap)
import           Data.Int                as X (Int32)
import           Data.List.Extra         as X (snoc)
import           Data.Maybe              as X (fromMaybe, isJust)
import           Data.Traversable        as X ()
import           Data.Sequence           as X (Seq, ViewR (..))
import           Data.Text               as X (Text)
import           Data.Tuple.Extra        as X (both)
import           Data.Typeable           as X (Typeable)
import           Data.UUID               as X (UUID)
import           Data.Vector             as X (Vector)
import           Data.Void               as X (Void)
import           Data.Word               as X (Word32, Word8)
import           Debug.Trace             as X (trace)
import           GHC.Generics            as X (Generic)
import           Text.Printf             as X (printf)
import           TextShow                as X
import           Util                    as X (uncurry4)

identity :: a -> a
identity x = x
