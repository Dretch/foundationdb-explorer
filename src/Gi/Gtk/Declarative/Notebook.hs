{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gi.Gtk.Declarative.Notebook
  ( NotebookChild (..)
  ) where

import           Control.Monad (void)
import           Data.Text (Text)
import           Data.Vector (Vector)
import qualified GI.Gtk as Gtk
import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Widget
import           GI.Gtk.Declarative.Patch

data NotebookChild event = NotebookChild
    { label :: Text
    , child :: Widget event
    }
    deriving (Functor)

instance Patchable NotebookChild where
    create = create . child
    patch s b1 b2 = patch s (child b1) (child b2)

instance EventSource NotebookChild where
    subscribe NotebookChild{..} = subscribe child

instance ToChildren Gtk.Notebook Vector NotebookChild

instance IsContainer Gtk.Notebook NotebookChild where
    appendChild notebook NotebookChild{label} widget = do
        lbl <- Gtk.labelNew (Just label)
        void $ Gtk.notebookAppendPage notebook widget (Just lbl)

    replaceChild notebook NotebookChild{label} i old new = do
        Gtk.widgetDestroy old
        lbl <- Gtk.labelNew (Just label)
        void $ Gtk.notebookInsertPage notebook new (Just lbl) i