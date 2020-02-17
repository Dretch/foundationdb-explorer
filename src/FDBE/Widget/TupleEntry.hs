{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A widget for editing foundationdb tuples
module FDBE.Widget.TupleEntry
  ( tupleEntry
  ) where

import           Data.ByteString            (ByteString)
import           Data.List.Extra            (snoc)
import           Data.List.Index            (deleteAt, setAt)
import           Data.Text                  (Text)
import qualified Data.UUID                  as UUID
import qualified Data.Vector                as Vector
import           Data.Word                  (Word32)
import           FoundationDB.Layer.Tuple   (Elem)
import qualified FoundationDB.Layer.Tuple   as LT
import           GI.Gdk                     (EventFocus)
import           GI.Gtk                     (Align (..), Box (..), Button (..),
                                             Entry (..), Label (..),
                                             Orientation (..), entryGetText)
import           GI.Gtk.Declarative
import           Util                       (uncurry4)

import           FDBE.Bytes                 (bytesToText, textToBytes)
import           FDBE.Widget.ComboBoxBool   (comboBoxBool)
import           FDBE.Widget.ComboBoxText   (comboBoxText)
import           FDBE.Widget.DoubleSpinner  (doubleSpinner)
import           FDBE.Widget.IntegerSpinner (integerSpinner)

tupleEntry
  :: Either Text [Elem]
  -> Bool
  -> (Either Text [Elem] -> event)
  -> Widget event
tupleEntry tuple sensitive onChange =
  container Box [#spacing := 2, #hexpand := True, #sensitive := sensitive]
    [ BoxChild defaultBoxChildProperties combo
    , BoxChild defaultBoxChildProperties { expand = True, fill = True } entry
    ]
  where
    combo = comboBoxText
      [#valign := AlignStart]
      ["Raw", "Tuple"]
      (Just comboIndex)
      (Just onTypeChange)

    comboIndex = either (const 0) (const 1) tuple

    onTypeChange i =
      case (i, tuple) of
        (0, Right t) ->
          onChange . Left . bytesToText $ LT.encodeTupleElems t
        (1, Left t) | Right tup <- LT.decodeTupleElems (textToBytes t) ->
          onChange $ Right tup
        (1, Left _) | otherwise ->
          onChange $ Right [LT.Text ""]
        _ ->
          onChange tuple

    entry =
      case tuple of
        Left text   -> rawEntry text (onChange . Left)
        Right elems -> tupleEntry' elems (onChange . Right)

rawEntry :: Text -> (Text -> event) -> Widget event
rawEntry text onRawChange =
  widget Entry
    [ #text := text
    , #tooltipText := escapeSyntaxHelp
    , onM #changed (fmap onRawChange . entryGetText)
    , #hexpand := True
    ]

tupleEntry' :: forall event. [Elem] -> ([Elem] -> event) -> Widget event
tupleEntry' elems onChange =
  container Box [#orientation := OrientationVertical, #spacing := 2]
    $ Vector.imap elemEntry (Vector.fromList elems) `Vector.snoc` addElemButton
  where
    elemEntry i field =
      BoxChild defaultBoxChildProperties { expand = True, fill = True }
        $ container Box [#spacing := 2]
          $ [ BoxChild defaultBoxChildProperties combo
            , BoxChild defaultBoxChildProperties { expand = True, fill = True } input
            , BoxChild defaultBoxChildProperties removeButton
            ]
      where
        combo = comboBoxText []
          ["None", "Bytes", "Text", "Int", "Float", "Double", "Bool", "UUID", "Tuple"]
          (Just position)
          (Just onElemTypeChange)

        removeButton =
          widget Button
            [ #label := "Remove"
            , on #clicked $ onChange $ deleteAt i elems
            ]

        (position, input) = case field of
          LT.None           -> (0, noneInput)
          LT.Bytes bs       -> (1, bytesInput bs)
          LT.Text t         -> (2, textInput t)
          LT.Int x          -> (3, intInput x)
          LT.Float f        -> (4, floatInput f)
          LT.Double d       -> (5, doubleInput d)
          LT.Bool b         -> (6, boolInput b)
          LT.UUID a b c d   -> (7, uuidInput a b c d)
          LT.CompleteVS _   -> (2, textInput "")
          LT.IncompleteVS _ -> (2, textInput "")
          LT.Tuple t        -> (8, tupleInput t)

        onElemTypeChange typePos =
          let newField =
                case typePos of
                  0 -> LT.None
                  1 -> LT.Bytes ""
                  2 -> LT.Text ""
                  3 -> LT.Int 0
                  4 -> LT.Float 0
                  5 -> LT.Double 0
                  6 -> LT.Bool False
                  7 -> LT.UUID 0 0 0 0
                  8 -> LT.Tuple []
                  _ -> error "invalid tuple element type position"
            in onChange $ setAt i newField elems

        noneInput =
          widget Label []

        bytesInput :: ByteString -> Widget event
        bytesInput bs =
          widget Entry
            [ #text := bytesToText bs
            , #tooltipText := escapeSyntaxHelp
            , #hexpand := True
            , onM #changed (fmap (onElemValueChange . LT.Bytes . textToBytes) . entryGetText)
            ]

        textInput :: Text -> Widget event
        textInput t =
          widget Entry
            [ #text := t
            , #hexpand := True
            , onM #changed (fmap (onElemValueChange . LT.Text) . entryGetText)
            ]

        boolInput b =
          comboBoxBool [] (Just b) (Just $ onElemValueChange . LT.Bool)

        floatInput f =
          doubleSpinner [] (realToFrac f) (onElemValueChange . LT.Float . realToFrac)

        doubleInput d =
          doubleSpinner [] d (onElemValueChange . LT.Double)

        intInput x =
          integerSpinner [] True x (onElemValueChange . LT.Int)

        uuidInput :: Word32 -> Word32 -> Word32 -> Word32 -> Widget event
        uuidInput a b c d =
          widget Entry
            [ #overwriteMode := True
            , #text := UUID.toText uuid
            , onM #focusOutEvent onFocusOut
            ]
          where
            uuid = UUID.fromWords a b c d
            onFocusOut :: EventFocus -> Entry -> IO (Bool, event)
            onFocusOut _ entry = do
              text <- #getText entry
              newUuid <- case UUID.fromText text of
                Just uuid' ->
                  pure uuid'
                Nothing -> do
                  -- the user entered an invalid UUID, so don't save it
                  #setText entry $ UUID.toText uuid
                  pure uuid
              pure (False, onElemValueChange . uncurry4 LT.UUID . UUID.toWords $ newUuid)

        tupleInput t =
          tupleEntry' t (onElemValueChange . LT.Tuple)

        onElemValueChange newField =
          onChange $ setAt i newField elems

    addElemButton =
      BoxChild defaultBoxChildProperties
        $ widget Button
        $ [ #label := "Add Element"
          , #halign := AlignStart
          , on #clicked (onChange $ elems `snoc` LT.Text "")
          ]

escapeSyntaxHelp :: Text
escapeSyntaxHelp =
  "Text will be UTF-8 encoded except for byte values specified in hex, like \
   \'\\xA0'. Use double slash '\\\\' to enter a single slash."
