{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module FDBE.App
  ( App(..)
  ) where


import           FDBE.Prelude

import           Control.Monad.State.Class                   (modify)
import           Data.HashSet                                (HashSet)
import qualified Data.HashSet                                as HashSet
import qualified Data.Vector                                 as Vector
import           FoundationDB                                (Database)
import           GI.Gtk                                      (Align (..),
                                                              Box (..),
                                                              Label (..),
                                                              MenuBar (..),
                                                              MenuItem (..),
                                                              Orientation (..),
                                                              Window (..),
                                                              WindowPosition (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Components
import           GI.Gtk.Declarative.Attributes.Custom.Window (window)
import           System.Random                               (randomIO)

import           FDBE.Component.KeyWindow
import           FDBE.Component.Search
import           FDBE.Component.StatusWindow

data App event = App Database (AppAction -> event)

instance Component App where

  data ComponentState App = AppState
    { statusVisible :: Bool
    , keyWindows :: HashSet KeyWindowId
    }

  data ComponentAction App
    = Close
    | ShowStatus
    | HideStatus
    | NewKeyWindow KeyWindowId
    | CloseKeyWindow KeyWindowId

  createComponent (App _db _cb) =
    ( AppState
        { statusVisible = False
        , keyWindows = mempty
        }
    , Nothing
    )
  
  update (App _db cb) = \case
    Close ->
      updateParent (cb Exit)
    ShowStatus ->
      modify (\s -> s{ statusVisible = True })
    HideStatus ->
      modify (\s -> s{ statusVisible = False })
    NewKeyWindow id->
      modify (\state -> state { keyWindows = HashSet.insert id (keyWindows state) })
    CloseKeyWindow id ->
      modify (\state -> state { keyWindows = HashSet.delete id (keyWindows state) })
  
  view (App database _cb) AppState{..} =
    bin
      Window
      ([ #title := "FoundationDB Explorer"
      , on #deleteEvent $ const (True, Close)
      , #widthRequest := 800
      , #heightRequest := 800
      , #windowPosition := WindowPositionCenter
      ] <> statusWindowAttr <> keyWindowAttrs) $
    container
      Box
      [#orientation := OrientationVertical]
      [ 
        container MenuBar []
          [ subMenu
              "Foundation DB"
              [ menuItem MenuItem [on #activate ShowStatus]
                  $ widget Label [#label := "Database Status", #halign := AlignStart]
              , menuItem MenuItem [onM #activate (const (NewKeyWindow <$> randomIO))]
                  $ widget Label [#label := "Edit Value at Key", #halign := AlignStart]
              ]
          ]
      , BoxChild
          { properties = defaultBoxChildProperties { fill = True, expand = True }
          , child = component (Search database)
          }
      ]
    where
      statusWindowAttr
        | statusVisible = [statusWindow database]
        | otherwise     = []
      keyWindowAttrs = Vector.fromList $
        HashSet.toList keyWindows <&> keyWindow database

statusWindow :: Database -> Attribute w (ComponentAction App)
statusWindow db = window () $ bin Window
  [ #title := "Database Status"
  , on #deleteEvent (const (True, HideStatus))
  , #windowPosition := WindowPositionCenter
  ] $
  component (StatusWindow db)

keyWindow :: Database -> KeyWindowId -> Attribute w (ComponentAction App)
keyWindow db id = window id $ bin Window
  [ #title := "Edit Value at Key"
  , on #deleteEvent (const (True, CloseKeyWindow id))
  , #widthRequest := 900
  , #heightRequest := 500
  , #windowPosition := WindowPositionCenter
  , classes ["keyWindow"]
  ] $
  component (KeyWindow db)
