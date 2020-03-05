{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FDBE.KeyWindow
  ( view'
  ) where

import           Data.Maybe                        (isJust)
import           Data.Vector                       (Vector)
import           GI.Gtk                            (Align (..), Box (..),
                                                    Button (..), Frame (..),
                                                    Grid (..), Label (..),
                                                    Orientation (..),
                                                    Window (..),
                                                    WindowPosition (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Container.Grid (GridChild (..),
                                                    GridChildProperties (..),
                                                    defaultGridChildProperties)

import           FDBE.Event                        (Event (..),
                                                    KeyWindowEvent (..))
import           FDBE.State                        (Operation (..),
                                                    EditableBytes,
                                                    KeyWindow (..),
                                                    operationSuccess)
import qualified FDBE.Widget.ComboBoxText          as ComboBoxText
import qualified FDBE.Widget.TupleEntry            as TupleEntry

view' :: Int -> KeyWindow -> Bin Window Event
view' i KeyWindow {..} =
  bin Window
    [ #title := "Edit Value at Key"
    , on #deleteEvent (const (True, KeyWindowEvent $ CloseKeyWindow i))
    , #widthRequest := 900
    , #heightRequest := 500
    , #windowPosition := WindowPositionCenter
    , classes ["keyWindow"]
    ] $
    container Grid
      [ #orientation := OrientationVertical
      , #margin := 4
      , #sensitive := (keyWindowSave /= OperationInProgress)
      ]
      [ GridChild
          { properties = defaultGridChildProperties { height = 2 }
          , child =
              bin Frame [#label := "Key", #vexpand := True, #hexpand := True, #margin := 2] $
                TupleEntry.tupleEntry
                  [ TupleEntry.Value keyWindowKey
                  , TupleEntry.OnChanged (KeyWindowEvent . UpdateKeyWindowKey i)
                  ]
          }
      , GridChild
          { properties = defaultGridChildProperties { leftAttach = 1 }
          , child =
              bin Frame
                [ #label := "Existing Value"
                , #hexpand := True
                , #margin := 2
                ] $
                oldValueEntry
                  keyWindowOldValue
                  (KeyWindowEvent $ LoadWindowKeyOldValue i keyWindowKey)
          }
      , GridChild
          { properties = defaultGridChildProperties { leftAttach = 1, topAttach = 1 }
          , child =
              bin Frame [#label := "New Value", #hexpand := True, #margin := 2] $
                newValueEntry
                  keyWindowNewValue
                  (KeyWindowEvent . UpdateKeyWindowNewValue i <$> operationSuccess keyWindowOldValue)
                  (KeyWindowEvent . UpdateKeyWindowNewValue i)
          }
      ]

oldValueEntry
  :: Operation (Maybe EditableBytes)
  -> event
  -> Widget event
oldValueEntry oldValue onLoadClick =
  container Box
    [ #orientation := OrientationVertical
    , #spacing := 4
    ] $
    [loadButton] <> loadedBoxChildren
  where
    loadButton = BoxChild
      { properties = defaultBoxChildProperties
      , child = widget Button
          [ #label := "Load value at key"
          , #halign := AlignStart
          , #sensitive := (oldValue /= OperationInProgress)
          , on #clicked onLoadClick
          ]
      }

    loadedBoxChildren = case oldValue of
      OperationNotStarted -> []
      OperationInProgress -> []
      OperationSuccess maybeValue ->
        let combo = existsCombo maybeValue [ComboBoxText.RawAttribute (#sensitive := False)]
            entry = case maybeValue of
              Nothing -> []
              Just mv ->
                [ BoxChild
                    { properties = defaultBoxChildProperties { expand = True, fill = True }
                    , child = TupleEntry.tupleEntry
                        [ TupleEntry.Value mv
                        , TupleEntry.RawAttribute (#sensitive := False)
                        ]
                    }
                ]
        in [combo] <> entry
      OperationFailure msg ->
        [ BoxChild
            { properties = defaultBoxChildProperties
            , child = widget Label [#label := msg]
            }
        ]

newValueEntry
  :: Maybe EditableBytes
  -> Maybe event
  -> (Maybe EditableBytes -> event)
  -> Widget event
newValueEntry maybeValue onCopyClick onChange =
  container Box
    [ #orientation := OrientationVertical
    , #spacing := 4
    ] $
    [copyButton, combo] <> entry <> [saveButton]
  where
    copyButton = BoxChild
      { properties = defaultBoxChildProperties
      , child = widget Button $
          [ #label := "Copy from existing value"
          , #halign := AlignStart
          ] <> case onCopyClick of
            Nothing -> [#sensitive := False]
            Just e  -> [on #clicked e]
      }

    combo =
      existsCombo maybeValue [ComboBoxText.OnChanged (onChange . onComboChange)]

    entry = case maybeValue of
      Nothing -> []
      Just mv ->
        [ BoxChild
            { properties = defaultBoxChildProperties { expand = True, fill = True }
            , child =
                TupleEntry.tupleEntry
                  [ TupleEntry.Value mv
                  , TupleEntry.OnChanged (onChange . Just)
                  ]
            }
        ]

    onComboChange pos =
      if pos == 0 then Just (Left "") else Nothing

    saveButton = BoxChild
      { properties = defaultBoxChildProperties
      , child = widget Button
          [ #label := "Save Value"
          , #halign := AlignEnd
          , #sensitive := False
          ]
      }

existsCombo
  :: Maybe EditableBytes
  -> Vector (ComboBoxText.ComboBoxAttribute event)
  -> BoxChild event
existsCombo maybeValue attrs =
  BoxChild
    { properties = defaultBoxChildProperties
    , child =
        ComboBoxText.comboBox $
          [ ComboBoxText.Choices ["Exists", "Doesn't Exist"]
          , ComboBoxText.Position (if isJust maybeValue then 0 else 1)
          ] <> attrs
    }
