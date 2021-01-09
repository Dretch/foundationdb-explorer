{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module FDBE.Component.StatusWindow
  ( StatusWindow(..)
  ) where

import FDBE.Prelude

import Control.Concurrent                          (threadDelay)
import Control.Monad.State.Class                   (put)
import FoundationDB                                (Database)
import GI.Gtk                                      (Align (..), Label (..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.Components

import FDBE.FoundationDB                           (getStatus)

newtype StatusWindow event = StatusWindow Database

instance Component StatusWindow where

  data ComponentAction StatusWindow = WaitThenLoadStatus Int | SetStatus Text

  data ComponentState StatusWindow = StatusWindowState Text

  createComponent (StatusWindow _db) =
    (StatusWindowState "...", Just (WaitThenLoadStatus 0))
  
  patchComponent state (StatusWindow _db) =
    state
  
  update (StatusWindow db) = \case
    WaitThenLoadStatus seconds ->
      updateIO $ do
        threadDelay $ seconds * 1000 * 1000
        Just . SetStatus <$> getStatus db
    SetStatus s -> do
      put $ StatusWindowState s
  
  view _decl (StatusWindowState status) =
    widget
      Label
      [ #label := status
      , classes ["status"]
      , #margin := 10
      , #halign := AlignStart
      , #valign := AlignStart
      ]
