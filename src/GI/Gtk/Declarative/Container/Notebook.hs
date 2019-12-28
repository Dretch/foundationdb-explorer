{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module GI.Gtk.Declarative.Container.Notebook
  ( Page
  , page
  , pageWithTab
  , notebook
  ) where

import           Control.Monad                      (void)
import           Data.Maybe                         (isNothing)
import           Data.Text                          (Text, pack)
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as Vector
import           GHC.Ptr                            (nullPtr)
import qualified GI.GLib                            as GLib
import qualified GI.Gtk                             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Container
import           GI.Gtk.Declarative.Container.Class

data Page event =
  Page
    { tabLabel :: Widget event
    , child    :: Widget event
    }

page :: Text -> Widget event -> Page event
page label = pageWithTab (widget Gtk.Label [#label := label])

pageWithTab :: Widget event -> Widget event -> Page event
pageWithTab = Page

notebook ::
     Vector (Attribute Gtk.Notebook event)
  -> Vector (Page event)
  -> Widget event
notebook attrs children =
  let tabsAndChildren = concat $ (\Page {..} -> [child, tabLabel]) <$> children
   in container Gtk.Notebook attrs $ Vector.fromList tabsAndChildren

instance ToChildren Gtk.Notebook Vector Widget

instance IsContainer Gtk.Notebook Widget where
  appendChild parent _ new = do
    lastPage <- Gtk.notebookGetNthPage parent (-1)
    case lastPage of
      Nothing -- this is the first page to be added
       -> do
        void $ Gtk.notebookAppendPage parent new (Nothing :: Maybe Gtk.Widget)
      Just p -> do
        label <- Gtk.notebookGetTabLabel parent p
        if isNothing label -- this page must already have a child, we just need to set the label
          then Gtk.notebookSetTabLabel parent p (Just new)
                                        -- the last page has a child and a label, so create a new page instead
          else void $
               Gtk.notebookAppendPage parent new (Nothing :: Maybe Gtk.Widget)
  replaceChild parent _ i old new = do
    let i' = i `div` 2
    pageI <- Gtk.notebookGetNthPage parent i'
    case pageI of
      Nothing -> do
        GLib.logDefaultHandler
          (Just "gi-gtk-declarative")
          [GLib.LogLevelFlagsLevelError]
          (Just $
           "Notebook.replaceChild called with an index where there is no child: " <>
           pack (show i))
          nullPtr
      Just p -> do
        if i `mod` 2 == 0 -- we have to replace the child
          then do
            label <- Gtk.notebookGetMenuLabel parent p
            Gtk.widgetDestroy old
            void $ Gtk.notebookInsertPage parent new label i'
          else do
            Gtk.notebookSetTabLabel parent p (Just new)
