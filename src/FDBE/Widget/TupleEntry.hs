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

import           Data.ByteString            (ByteString)
import           Data.List.Extra            (snoc)
import           Data.List.Index            (deleteAt, setAt)
import           Data.Text                  (Text)
import qualified Data.UUID                  as UUID
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import           Data.Word                  (Word32)
import           FoundationDB.Layer.Tuple   (Elem)
import qualified FoundationDB.Layer.Tuple   as LT
import           FoundationDB.Versionstamp  (TransactionVersionstamp (..),
                                             Versionstamp (..),
                                             VersionstampCompleteness (..))
import           GI.Gdk                     (EventFocus)
import           GI.Gtk                     (Align (..), Box (..), Button (..),
                                             Entry (..), Label (..),
                                             Orientation (..), entryGetText)
import           GI.Gtk.Declarative
import           Util                       (uncurry4)

import           FDBE.Bytes                 (bytesToText, textToBytes)
import qualified FDBE.Widget.ComboBoxBool   as ComboBoxBool
import qualified FDBE.Widget.ComboBoxText   as ComboBoxText
import qualified FDBE.Widget.DoubleSpinner  as DoubleSpinner
import qualified FDBE.Widget.IntegerSpinner as IntegerSpinner

data TupleEntryAttribute event
  = RawAttribute (Attribute Box event)
  | Value (Either Text [Elem])
  | OnChanged (Either Text [Elem] -> event)

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
          onChange . Left . bytesToText $ LT.encodeTupleElems t
        (1, Left t) | Right tup <- LT.decodeTupleElems (textToBytes t) ->
          onChange $ Right tup
        (1, Left _) ->
          onChange $ Right [LT.Text ""]
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

tupleEntry' :: forall event. [Elem] -> Vector ([Elem] -> event) -> Widget event
tupleEntry' elems onChange =
  container Box [#orientation := OrientationVertical, #spacing := 2]
    $ Vector.imap elemEntry (Vector.fromList elems) `Vector.snoc` addElemButton
  where
    elemEntry :: Int -> Elem -> BoxChild event
    elemEntry i field =
      BoxChild defaultBoxChildProperties { expand = True, fill = True } $
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

        (position, input) = case field of
          LT.None           -> (0, noneInput)
          LT.Bytes bs       -> (1, bytesInput bs)
          LT.Text t         -> (2, textInput t)
          LT.Int x          -> (3, intInput $ fromIntegral x)
          LT.Float f        -> (4, floatInput f)
          LT.Double d       -> (5, doubleInput d)
          LT.Bool b         -> (6, boolInput b)
          LT.UUID a b c d   -> (7, uuidInput a b c d)
          LT.CompleteVS vs  -> (8, completeVsInput vs)
          LT.IncompleteVS _ -> (8, completeVsInput minBound)
          LT.Tuple t        -> (9, tupleInput t)

        onElemTypeChange :: Int -> [Elem]
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
                  8 -> LT.CompleteVS minBound
                  9 -> LT.Tuple []
                  _ -> error "invalid tuple element type position"
            in setAt i newField elems

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
            ] <> onElemValueChangeM (onM #changed) (fmap (LT.Bytes . textToBytes) . entryGetText)

        textInput :: Text -> Widget event
        textInput t =
          widget Entry $
            [ #text := t
            , #hexpand := True
            , #valign := AlignStart
            ] <> onElemValueChangeM (onM #changed) (fmap LT.Text . entryGetText)

        boolInput :: Bool -> Widget event
        boolInput b =
          ComboBoxBool.comboBox $
            [ ComboBoxBool.Position b
            ] <> onElemValueChange ComboBoxBool.OnChanged LT.Bool

        floatInput :: Float -> Widget event
        floatInput f =
          DoubleSpinner.spinner $
            [ DoubleSpinner.Value (realToFrac f)
            ] <> onElemValueChange DoubleSpinner.OnChanged (LT.Float . realToFrac)

        doubleInput :: Double -> Widget event
        doubleInput d =
          DoubleSpinner.spinner $
            [ DoubleSpinner.Value d
            ] <> onElemValueChange DoubleSpinner.OnChanged LT.Double

        intInput :: Integer -> Widget event
        intInput x =
          IntegerSpinner.spinner $
            [ IntegerSpinner.Value (fromIntegral x :: Int)
            ] <> onElemValueChange IntegerSpinner.OnChanged (LT.Int . fromIntegral)

        uuidInput :: Word32 -> Word32 -> Word32 -> Word32 -> Widget event
        uuidInput a b c d =
          widget Entry $
            [ #overwriteMode := True
            , #text := UUID.toText uuid
            ] <> (onM #focusOutEvent . onFocusOut <$> onChange)
          where
            uuid = UUID.fromWords a b c d
            onFocusOut :: ([Elem] -> event) -> EventFocus -> Entry -> IO (Bool, event)
            onFocusOut onChange' _ entry = do
              text <- #getText entry
              newUuid <- case UUID.fromText text of
                Just uuid' ->
                  pure uuid'
                Nothing -> do
                  -- the user entered an invalid UUID, so don't save it
                  #setText entry $ UUID.toText uuid
                  pure uuid
              let uuidElem = uncurry4 LT.UUID . UUID.toWords $ newUuid
              pure (False, onChange' $ setAt i uuidElem elems)

        completeVsInput :: Versionstamp 'Complete -> Widget event
        completeVsInput (CompleteVersionstamp (TransactionVersionstamp tx batch) usr) =
          container Box [#spacing := 4]
            [ label "Tx:"
            , spin $ IntegerSpinner.spinner $
                [ IntegerSpinner.Value tx
                ] <> onElemValueChange IntegerSpinner.OnChanged (LT.CompleteVS . flip CompleteVersionstamp usr . flip TransactionVersionstamp batch)
            , label "Batch:"
            , spin $ IntegerSpinner.spinner $
                [ IntegerSpinner.Value batch
                ] <> onElemValueChange IntegerSpinner.OnChanged (LT.CompleteVS . flip CompleteVersionstamp usr . TransactionVersionstamp tx)
            , label "User:"
            , spin $ IntegerSpinner.spinner $
                [ IntegerSpinner.Value usr
                ] <> onElemValueChange IntegerSpinner.OnChanged (LT.CompleteVS . CompleteVersionstamp (TransactionVersionstamp tx batch))
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

        tupleInput :: [Elem] -> Widget event
        tupleInput t =
          tupleEntry' t $ onElemValueChange id LT.Tuple

        onElemValueChange :: ((x -> event) -> attr event) -> (x -> Elem) -> Vector (attr event)
        onElemValueChange wrap convert =
          (\f -> wrap (f . replaceElem . convert)) <$> onChange

        onElemValueChangeM :: ((x -> IO event) -> attr event) -> (x -> IO Elem) -> Vector (attr event)
        onElemValueChangeM wrap convert =
          (\f -> wrap (fmap (f . replaceElem) . convert)) <$> onChange

        replaceElem :: Elem -> [Elem]
        replaceElem newElem =
          setAt i newElem elems

    addElemButton :: BoxChild event
    addElemButton =
      BoxChild defaultBoxChildProperties $
        widget Button $
        [ #label := "Add Element"
        , #halign := AlignStart
        ] <> ((\f -> on #clicked (f $ elems `snoc` LT.Text "")) <$> onChange)

escapeSyntaxHelp :: Text
escapeSyntaxHelp =
  "Text will be UTF-8 encoded except for byte values specified in hex, like \
   \'\\xA0'. Use double slash '\\\\' to enter a single slash."
