{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | A component for editing foundationdb tuples
module FDBE.Component.TupleEntry
  ( tupleEntry
  , tupleEntryV
  ) where

import FDBE.Prelude
import FDBE.FoundationDB
import FDBE.Bytes (textToBytes, bytesToText)
import Control.Lens hiding (imap, snoc)
import Data.List.Index (deleteAt, imap, setAt)
import Monomer
import GHC.Float (double2Float, float2Double)
import qualified FoundationDB.Layer.Tuple as LT
import qualified Data.UUID as UUID
import FoundationDB.Versionstamp (Versionstamp(CompleteVersionstamp), TransactionVersionstamp (TransactionVersionstamp), VersionstampCompleteness (Complete))

newtype TupleEntryEvent = TupleValueChanged EditableBytes

tupleEntry
 :: (CompositeEvent ep, CompParentModel sp)
 => ALens' sp EditableBytes
 -> WidgetNode sp ep
tupleEntry ls =
  composite widgetType ls buildUI (handleEvent Nothing)

tupleEntryV
 :: (CompositeEvent ep, CompParentModel sp)
 => EditableBytes
 -> (EditableBytes -> ep)
 -> WidgetNode sp ep
tupleEntryV value changeHandler =
  compositeD_ widgetType (WidgetValue value) buildUI (handleEvent (Just changeHandler)) []

widgetType :: WidgetType
widgetType = "FBDE.TupleEntry"

buildUI :: UIBuilder EditableBytes TupleEntryEvent
buildUI _wenv model = tree where
  tree = hstack_ [childSpacing_ 2] [
      textDropdownSV (getEditAs model) (TupleValueChanged . setEditAs model) [EditAsRaw, EditAsTuple]
        `styleBasic` [sizeReqW (fixedSize 60)],
      editor
    ]

  editor = case model of
    Left t ->
      tooltip escapeSyntaxHelp (textFieldV t (TupleValueChanged . Left))
    Right elems ->
      tupleEntry' elems (TupleValueChanged . Right)

tupleEntry'
 :: forall s e. (WidgetModel s, WidgetEvent e)
 => [EditableElem]
 -> ([EditableElem] -> e)
 -> WidgetNode s e
tupleEntry' elems elmsChange =
    vstack_ [childSpacing_ 2] $
      imap elemEntry elems <> [addElemButton]
  where

    elemEntry :: Int -> EditableElem -> WidgetNode s e
    elemEntry i elm = elemTree where
      elemTree = hstack_ [childSpacing_ 2] [
          elemTypeCombo,
          elemInput,
          removeElemButton
        ]

      elemTypeCombo =
        textDropdownSV (getElemType elm) (\et -> elmsChange (setAt i (setElemType elm et) elems)) (enumFrom None')
          `styleBasic` [sizeReqW (fixedSize 100)]

      elemInput = case elm of
        None             -> noneInput
        Bytes b          -> bytesInput b
        SingleLineText t -> singleLineTextInput t
        MultiLineText t  -> multiLineTextInput t
        Int x            -> intInput x
        Float f          -> floatInput f
        Double d         -> doubleInput d
        Bool b           -> boolInput b
        UUID a b c d     -> uuidInput a b c d
        CompleteVS vs    -> completeVSInput vs
        IncompleteVS _   -> completeVSInput minBound
        Tuple t          -> tupleInput t

      noneInput =
        label ""
      
      bytesInput b =
        -- todo: fix weirdness
        tooltip escapeSyntaxHelp $
          textFieldV (bytesToText b) (onElemChange . Bytes . textToBytes)

      singleLineTextInput t =
        textFieldV t (onElemChange . SingleLineText)
      
      multiLineTextInput t =
        textAreaV t (onElemChange . MultiLineText)

      intInput x =
        numericFieldV_ x (onElemChange . Int) [wheelRate 10]

      floatInput f =
        numericFieldV_ f (onElemChange . Float) [wheelRate 10]
      
      doubleInput d =
        numericFieldV_ d (onElemChange . Double) [wheelRate 10]
      
      boolInput b =
        textDropdownSV b (onElemChange . Bool) [False, True]
      
      -- todo: make less weird!
      uuidInput a b c d =
        -- maxlength, replacemode? validity check?
        textFieldD_ (WidgetValue (UUID.toText uuid)) [maxLength 36, onChange onChangeEvt]
        where
          uuid = UUID.fromWords a b c d
          onChangeEvt t =
            -- if the user entered an invalid UUID then don't save it
            let newUuid = fromMaybe uuid (UUID.fromText t)
                uuidElem = uncurry4 UUID . UUID.toWords $ newUuid
            in onElemChange uuidElem
      
      completeVSInput :: Versionstamp 'Complete -> WidgetNode s e
      completeVSInput (CompleteVersionstamp (TransactionVersionstamp tx batch) usr) =
        hstack_ [childSpacing_ 2] [
          label "Tx:",
          numericFieldV_ tx (onElemChange . CompleteVS . flip CompleteVersionstamp usr . flip TransactionVersionstamp batch) [wheelRate 10],
          label "Batch:",
          numericFieldV_ batch (onElemChange . CompleteVS . flip CompleteVersionstamp usr . TransactionVersionstamp tx) [wheelRate 10],
          label "User:",
          numericFieldV_ usr (onElemChange . CompleteVS . CompleteVersionstamp (TransactionVersionstamp tx batch)) [wheelRate 10]
        ]
      
      tupleInput t =
        tupleEntry' t (onElemChange . Tuple)
      
      onElemChange newVal =
        elmsChange (setAt i newVal elems)

      removeElemButton =
        button "Remove" (elmsChange (deleteAt i elems))

    addElemButton =
      button "Add Element" (elmsChange (elems `snoc` SingleLineText ""))

handleEvent :: Maybe (EditableBytes -> ep) -> EventHandler EditableBytes TupleEntryEvent sp ep
handleEvent changeHandler _wenv _node _model (TupleValueChanged newValue) =
  [Model newValue] <> case changeHandler of
    Just h -> [Report (h newValue)]
    Nothing -> []

data EditAs = EditAsRaw | EditAsTuple
  deriving (Eq, Enum)

instance Show EditAs where
  show EditAsRaw = "Raw"
  show EditAsTuple = "Tuple"

getEditAs :: EditableBytes -> EditAs
getEditAs (Left _) = EditAsRaw
getEditAs (Right _) = EditAsTuple

setEditAs :: EditableBytes -> EditAs -> EditableBytes
setEditAs eb ea = case (eb, ea) of
    (Left t,  EditAsRaw)   -> Left t
    (Left t,  EditAsTuple) | Right tp <- LT.decodeTupleElems (textToBytes t) -> Right (toEditableElem <$> tp)
    (Left _,  EditAsTuple) -> Right []
    (Right t, EditAsTuple) -> Right t
    (Right t, EditAsRaw)   -> Left . bytesToText . LT.encodeTupleElems $ fromEditableElem <$> t

data ElemType
  = None'
  | Tuple'
  | Bytes'
  | SingleLineText'
  | MultiLineText'
  | Int'
  | Float'
  | Double'
  | Bool'
  | UUID'
  | VersionStamp'
  deriving (Eq, Enum)

instance Show ElemType where
    show None' = "None"
    show Tuple' = "Tuple"
    show Bytes' = "Bytes"
    show SingleLineText' = "Text"
    show MultiLineText' = "Text (long)"
    show Float' = "Float"
    show Double' = "Double"
    show Int' = "Integer"
    show Bool' = "Boolean"
    show UUID' = "UUID"
    show VersionStamp' = "Versionstamp"

getElemType :: EditableElem -> ElemType
getElemType = \case
  None -> None'
  Tuple _ -> Tuple'
  Bytes _ -> Bytes'
  SingleLineText _-> SingleLineText'
  MultiLineText _ -> MultiLineText'
  Int _ -> Int'
  Float _ -> Float'
  Double _ -> Double'
  Bool _ -> Bool'
  UUID {} -> UUID'
  CompleteVS _ -> VersionStamp'
  IncompleteVS _ -> VersionStamp'

setElemType :: EditableElem -> ElemType -> EditableElem
setElemType elm = \case
  None' ->
    None
  Tuple' ->
    case elm of
      Tuple t -> Tuple t
      _ -> Tuple []
  Bytes' ->
      case elm of
          Bytes b -> Bytes b
          _       -> Bytes ""
  SingleLineText' | Just t <- elemText elm ->
    SingleLineText t
  SingleLineText' ->
    SingleLineText ""
  MultiLineText' | Just t <- elemText elm ->
    MultiLineText t
  MultiLineText' ->
    MultiLineText ""
  Int' ->
    case elm of
      Int i -> Int i
      Float f -> Int (truncate f)
      Double d -> Int (truncate d)
      _     -> Int 0
  Float' ->
    case elm of
      Float f -> Float f
      Double d -> Float (double2Float d)
      Int i -> Float (fromIntegral i)
      _ -> Float 0
  Double' ->
    case elm of
      Float f -> Double (float2Double f)
      Double d -> Double d
      Int i -> Double (fromIntegral i)
      _ -> Double 0
  Bool' ->
    case elm of
      Bool b -> Bool b
      _      -> Bool False
  UUID' ->
    case elm of
      UUID a b c d -> UUID a b c d
      _ -> UUID 0 0 0 0
  VersionStamp' ->
    case elm of
      CompleteVS vs -> CompleteVS vs
      _ -> CompleteVS minBound

escapeSyntaxHelp :: Text
escapeSyntaxHelp =
  "Text will be UTF-8 encoded except for byte values specified in hex, like \
   \'\\xA0'. Use double slash '\\\\' to enter a single slash."
