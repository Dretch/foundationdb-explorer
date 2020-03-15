{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FDBE.KeyWindow
  ( view'
  ) where

import           Prelude                           hiding (id)

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
import           FDBE.State                        (EditableBytes,
                                                    KeyWindow (..), KeyWindowId,
                                                    Operation (..),
                                                    operationSuccess)
import qualified FDBE.Widget.ComboBoxText          as ComboBoxText
import qualified FDBE.Widget.TupleEntry            as TupleEntry

view' :: KeyWindowId -> KeyWindow -> Bin Window Event
view' id KeyWindow {..} =
  bin Window
    [ #title := "Edit Value at Key"
    , on #deleteEvent (const (True, KeyWindowEvent $ CloseKeyWindow id))
    , #widthRequest := 900
    , #heightRequest := 500
    , #windowPosition := WindowPositionCenter
    , classes ["keyWindow"]
    ] $
    container Grid
      [ #orientation := OrientationVertical
      , #margin := 4
      , #rowHomogeneous := True
      , #columnHomogeneous := True
      ]
      [ GridChild
          { properties = defaultGridChildProperties { height = 2 }
          , child =
              bin Frame [#label := "Key", #vexpand := True, #hexpand := True, #margin := 2] $
                TupleEntry.tupleEntry
                  [ TupleEntry.Value keyWindowKey
                  , TupleEntry.OnChanged (KeyWindowEvent . UpdateKeyWindowKey id)
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
                oldValueEntry id keyWindowKey keyWindowOldValue
          }
      , GridChild
          { properties = defaultGridChildProperties { leftAttach = 1, topAttach = 1 }
          , child =
              bin Frame [#label := "New Value", #hexpand := True, #margin := 2] $
                newValueEntry id keyWindowKey keyWindowOldValue keyWindowNewValue keyWindowSave
          }
      ]

oldValueEntry
  :: KeyWindowId
  -> EditableBytes
  -> Operation (Maybe EditableBytes)
  -> Widget Event
oldValueEntry id key oldValue =
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
          , on #clicked (KeyWindowEvent $ LoadKeyWindowOldValue id key)
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
            { properties = defaultBoxChildProperties { expand = True, fill = False }
            , child = widget Label [#label := msg]
            }
        ]

newValueEntry
  :: KeyWindowId
  -> EditableBytes
  -> Operation (Maybe EditableBytes)
  -> Maybe EditableBytes
  -> Operation ()
  -> Widget Event
newValueEntry id key oldValue newValue saveOperation =
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
          ] <> case operationSuccess oldValue of
            Nothing -> [#sensitive := False]
            Just e  -> [on #clicked (KeyWindowEvent $ UpdateKeyWindowNewValue id e)]
      }

    onChange =
      KeyWindowEvent . UpdateKeyWindowNewValue id

    combo =
      existsCombo newValue [ComboBoxText.OnChanged (onChange . onComboChange)]

    entry = case newValue of
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
      { properties = defaultBoxChildProperties {expand = True, fill = True}
      , child = container Box [#valign := AlignEnd]
          [ BoxChild
              { properties = defaultBoxChildProperties { expand = True, fill = True }
              , child = widget Label
                  [ #label := saveResultLabel
                  , #halign := AlignStart
                  ]
              }
          , BoxChild
              { properties = defaultBoxChildProperties
              , child = widget Button $
                  [ #label := "Save Value"
                  , #halign := AlignEnd
                  ] <> case saveOperation of
                    OperationInProgress -> [#sensitive := False]
                    _ -> [on #clicked (KeyWindowEvent $ KeyWindowSave id key newValue)]
              }
          ]
      }

    saveResultLabel = case saveOperation of
      OperationNotStarted  -> ""
      OperationInProgress  -> ""
      OperationFailure msg -> msg
      OperationSuccess ()  -> "Value saved successfully"

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
