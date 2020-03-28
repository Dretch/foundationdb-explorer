{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A widget for editing foundationdb tuples
module FDBE.Widget.TupleEntry
  ( TupleEntryAttribute(..)
  , tupleEntry
  ) where

import           FDBE.Prelude

import           Data.List.Index            (deleteAt, setAt)
import qualified Data.UUID                  as UUID
import qualified Data.Vector                as Vector
import qualified FoundationDB.Layer.Tuple   as LT
import           FoundationDB.Versionstamp  (TransactionVersionstamp (..),
                                             Versionstamp (..),
                                             VersionstampCompleteness (..))
import           GI.Gdk                     (EventFocus)
import           GI.Gtk                     (Align (..), Box (..), Button (..),
                                             Entry (..), Label (..),
                                             Orientation (..), entryGetText)
import           GI.Gtk.Declarative

import           FDBE.Bytes                 (bytesToText, textToBytes)
import           FDBE.State                 (EditableBytes, EditableElem (..),
                                             elemText, fromEditableElem,
                                             toEditableElem)
import qualified FDBE.Widget.ComboBoxBool   as ComboBoxBool
import qualified FDBE.Widget.ComboBoxText   as ComboBoxText
import qualified FDBE.Widget.DoubleSpinner  as DoubleSpinner
import qualified FDBE.Widget.IntegerSpinner as IntegerSpinner
import qualified FDBE.Widget.TextView       as TextView

data TupleEntryAttribute event
  = RawAttribute (Attribute Box event)
  | Value EditableBytes
  | OnChanged (EditableBytes -> event)

tupleEntry :: Vector (TupleEntryAttribute event) -> Widget event
tupleEntry attrs =
  container Box (rawAttrs <> [#spacing := 2, #hexpand := True])
    [ BoxChild defaultBoxChildProperties combo
    , BoxChild defaultBoxChildProperties { expand = True, fill = True } entry
    ]
  where
    (rawAttrs, value, onChanged) = foldl go (Vector.empty, Left "", Vector.empty) attrs

    go (attrs', value', onChanged') = \case
      RawAttribute a -> (a `Vector.cons` attrs', value', onChanged')
      Value x        -> (attrs', x, onChanged')
      OnChanged f    -> (attrs', value', f `Vector.cons` onChanged')

    combo = ComboBoxText.comboBox $
      [ ComboBoxText.RawAttribute (#valign := AlignStart)
      , ComboBoxText.Choices ["Raw", "Tuple"]
      , ComboBoxText.Position comboIndex
      ] <> (ComboBoxText.OnChanged . onTypeChange <$> onChanged)

    comboIndex = either (const 0) (const 1) value

    onTypeChange onChange i =
      case (i, value) of
        (0, Right t) ->
          onChange . Left . bytesToText . LT.encodeTupleElems $ fromEditableElem <$> t
        (1, Left t) | Right tup <- LT.decodeTupleElems (textToBytes t) ->
          onChange . Right $ toEditableElem <$> tup
        (1, Left _) ->
          onChange $ Right [SingleLineText ""]
        _ ->
          onChange value

    entry =
      case value of
        Left text   -> rawEntry text $ (. Left) <$> onChanged
        Right elems -> tupleEntry' elems $ (. Right) <$> onChanged

rawEntry :: Text -> Vector (Text -> event) -> Widget event
rawEntry text onRawChange =
  widget Entry $
    [ #text := text
    , #tooltipText := escapeSyntaxHelp
    , #hexpand := True
    , #valign := AlignStart
    ] <> ((\f -> onM #changed (fmap f . entryGetText)) <$> onRawChange)

tupleEntry'
  :: forall event
   . [EditableElem]
  -> Vector ([EditableElem] -> event)
  -> Widget event
tupleEntry' elems onChange =
  container Box [#orientation := OrientationVertical, #spacing := 2]
    $ Vector.imap elemEntry (Vector.fromList elems) `Vector.snoc` addElemButton
  where
    elemEntry :: Int -> EditableElem -> BoxChild event
    elemEntry i field =
      BoxChild defaultBoxChildProperties $
        container Box [#spacing := 2]
          [ BoxChild defaultBoxChildProperties combo
          , BoxChild defaultBoxChildProperties { expand = True, fill = True } input
          , BoxChild defaultBoxChildProperties removeButton
          ]
      where
        combo :: Widget event
        combo =
          ComboBoxText.comboBox $
            [ ComboBoxText.Choices
                [ "None"
                , "Bytes"
                , "Text"
                , "Text (long)"
                , "Int"
                , "Float"
                , "Double"
                , "Bool"
                , "UUID"
                , "Versionstamp"
                , "Tuple"
                ]
            , ComboBoxText.Position position
            ] <> (ComboBoxText.OnChanged . (. onElemTypeChange) <$> onChange)

        removeButton :: Widget event
        removeButton =
          widget Button $
            [ #label := "Remove"
            ] <> ((\f -> on #clicked (f $ deleteAt i elems)) <$> onChange)

        position :: Int
        input :: Widget event
        (position, input) = case field of
          None             -> (0, noneInput)
          Bytes bs         -> (1, bytesInput bs)
          SingleLineText t -> (2, singleLineTextInput t)
          MultiLineText t  -> (3, multiLineTextInput t)
          Int x            -> (4, intInput $ fromIntegral x)
          Float f          -> (5, floatInput f)
          Double d         -> (6, doubleInput d)
          Bool b           -> (7, boolInput b)
          UUID a b c d     -> (8, uuidInput a b c d)
          CompleteVS vs    -> (9, completeVsInput vs)
          IncompleteVS _   -> (9, completeVsInput minBound)
          Tuple t          -> (10, tupleInput t)

        onElemTypeChange :: Int -> [EditableElem]
        onElemTypeChange typePos = setAt i newField elems
          where
            newField = case typePos of
              0  -> None
              1  -> Bytes ""
              2  -> SingleLineText (fromMaybe "" (elemText field))
              3  -> MultiLineText (fromMaybe "" (elemText field))
              4  -> Int 0
              5  -> Float 0
              6  -> Double 0
              7  -> Bool False
              8  -> UUID 0 0 0 0
              9  -> CompleteVS minBound
              10 -> Tuple []
              _  -> error "invalid tuple element type position - this is a bug"

        noneInput :: Widget event
        noneInput =
          widget Label []

        bytesInput :: ByteString -> Widget event
        bytesInput bs =
          widget Entry $
            [ #text := bytesToText bs
            , #tooltipText := escapeSyntaxHelp
            , #hexpand := True
            , #valign := AlignStart
            ] <> onElemValueChangeM (onM #changed) (fmap (Bytes . textToBytes) . entryGetText)

        singleLineTextInput :: Text -> Widget event
        singleLineTextInput t =
          widget Entry $
            [ #text := t
            , #hexpand := True
            , #valign := AlignStart
            ] <> onElemValueChangeM (onM #changed) (fmap SingleLineText . entryGetText)

        multiLineTextInput :: Text -> Widget event
        multiLineTextInput t =
          TextView.textView $
            [ TextView.RawAttribute (#hexpand := True)
            , TextView.RawAttribute (#valign := AlignStart)
            , TextView.Value t
            ] <> onElemValueChange TextView.OnChanged MultiLineText

        boolInput :: Bool -> Widget event
        boolInput b =
          ComboBoxBool.comboBox $
            [ ComboBoxBool.Position b
            ] <> onElemValueChange ComboBoxBool.OnChanged Bool

        floatInput :: Float -> Widget event
        floatInput f =
          DoubleSpinner.spinner $
            [ DoubleSpinner.Value (realToFrac f)
            ] <> onElemValueChange DoubleSpinner.OnChanged (Float . realToFrac)

        doubleInput :: Double -> Widget event
        doubleInput d =
          DoubleSpinner.spinner $
            [ DoubleSpinner.Value d
            ] <> onElemValueChange DoubleSpinner.OnChanged Double

        intInput :: Integer -> Widget event
        intInput x =
          IntegerSpinner.spinner $
            [ IntegerSpinner.Value (fromIntegral x :: Int)
            ] <> onElemValueChange IntegerSpinner.OnChanged (Int . fromIntegral)

        uuidInput :: Word32 -> Word32 -> Word32 -> Word32 -> Widget event
        uuidInput a b c d =
          widget Entry $
            [ #overwriteMode := True
            , #text := UUID.toText uuid
            ] <> (onM #focusOutEvent . onFocusOut <$> onChange)
          where
            uuid = UUID.fromWords a b c d
            onFocusOut :: ([EditableElem] -> event) -> EventFocus -> Entry -> IO (Bool, event)
            onFocusOut onChange' _ entry = do
              text <- #getText entry
              newUuid <- case UUID.fromText text of
                Just uuid' ->
                  pure uuid'
                Nothing -> do
                  -- the user entered an invalid UUID, so don't save it
                  #setText entry $ UUID.toText uuid
                  pure uuid
              let uuidElem = uncurry4 UUID . UUID.toWords $ newUuid
              pure (False, onChange' $ setAt i uuidElem elems)

        completeVsInput :: Versionstamp 'Complete -> Widget event
        completeVsInput (CompleteVersionstamp (TransactionVersionstamp tx batch) usr) =
          container Box [#spacing := 4]
            [ label "Tx:"
            , spin $ IntegerSpinner.spinner $
                [ IntegerSpinner.Value tx
                ] <> onElemValueChange IntegerSpinner.OnChanged (CompleteVS . flip CompleteVersionstamp usr . flip TransactionVersionstamp batch)
            , label "Batch:"
            , spin $ IntegerSpinner.spinner $
                [ IntegerSpinner.Value batch
                ] <> onElemValueChange IntegerSpinner.OnChanged (CompleteVS . flip CompleteVersionstamp usr . TransactionVersionstamp tx)
            , label "User:"
            , spin $ IntegerSpinner.spinner $
                [ IntegerSpinner.Value usr
                ] <> onElemValueChange IntegerSpinner.OnChanged (CompleteVS . CompleteVersionstamp (TransactionVersionstamp tx batch))
            ]
            where
              label t = BoxChild
                { properties = defaultBoxChildProperties
                , child = widget Label [#label := t]
                }
              spin child = BoxChild
                { properties = defaultBoxChildProperties { expand = True, fill = True }
                , child
                }

        tupleInput :: [EditableElem] -> Widget event
        tupleInput t =
          tupleEntry' t $ onElemValueChange identity Tuple

        onElemValueChange :: ((x -> event) -> attr event) -> (x -> EditableElem) -> Vector (attr event)
        onElemValueChange wrap convert =
          (\f -> wrap (f . replaceElem . convert)) <$> onChange

        onElemValueChangeM :: ((x -> IO event) -> attr event) -> (x -> IO EditableElem) -> Vector (attr event)
        onElemValueChangeM wrap convert =
          (\f -> wrap (fmap (f . replaceElem) . convert)) <$> onChange

        replaceElem :: EditableElem -> [EditableElem]
        replaceElem newElem =
          setAt i newElem elems

    addElemButton :: BoxChild event
    addElemButton =
      BoxChild defaultBoxChildProperties $
        widget Button $
        [ #label := "Add Element"
        , #halign := AlignStart
        ] <> ((\f -> on #clicked (f $ elems `snoc` SingleLineText "")) <$> onChange)

escapeSyntaxHelp :: Text
escapeSyntaxHelp =
  "Text will be UTF-8 encoded except for byte values specified in hex, like \
   \'\\xA0'. Use double slash '\\\\' to enter a single slash."
