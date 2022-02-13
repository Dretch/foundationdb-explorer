{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module FDBE.State
  ( Operation (..),
    operationSuccess,
  )
where

import FDBE.Prelude

-- | Some kind of operation that takes time, and that we want to update the UI
-- about before it has been completed. E.g. loading/saving database keys.
data Operation a
  = OperationNotStarted
  | OperationInProgress
  | OperationSuccess a
  | OperationFailure Text
  deriving (Eq, Show)

operationSuccess :: Operation a -> Maybe a
operationSuccess = \case
  OperationSuccess a -> Just a
  _ -> Nothing
