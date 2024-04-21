{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Row (
  row,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Alignment
import Iced.Attribute.AlignmentFFI
import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Spacing
import Iced.Attribute.Padding
import Iced.Element

data NativeRow
type Self = Ptr NativeRow
type AttributeFn = Self -> IO Self

data Attribute
  = Spacing Float
  | AddPadding Padding
  | AlignItems Alignment
  | Width Length
  | Height Length

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_row"
--  new_row :: IO Self

foreign import ccall safe "row_align_items"
  row_align_items :: Self -> AlignmentPtr -> IO Self

-- row top right bottom left
foreign import ccall safe "row_padding"
  row_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

-- row value
foreign import ccall safe "row_spacing"
  row_spacing :: Self -> CFloat -> IO Self

foreign import ccall safe "row_with_children"
  row_with_children :: CUInt -> Ptr ElementPtr -> IO Self

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "row_extend"
--  row_extend :: Self -> CUInt -> Ptr ElementPtr -> IO Self

foreign import ccall safe "row_width"
  row_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "row_height"
  row_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "row_into_element"
  row_into_element :: Self -> IO ElementPtr

data Row = Row { attributes :: [Attribute], children :: [Element] }

instance IntoNative Row where
  toNative details = do
    elements <- buildElements details.children []
    let len = fromIntegral $ length elements
    elementsPtr <- newArray elements
    self <- row_with_children len elementsPtr
    free elementsPtr
    updatedSelf <- applyAttributes self details.attributes
    row_into_element updatedSelf

instance UseAttribute Self Attribute where
  useAttribute self attribute = do
    case attribute of
      Spacing value -> useSpacing value self
      AddPadding value -> usePadding value self
      AlignItems value -> useAlignItems value self
      Width len -> useWidth len self
      Height len -> useHeight len self

instance UseAlignment Attribute where
  alignItems value = AlignItems value

instance UsePadding Attribute where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute Attribute where
  paddingToAttribute value = AddPadding value

instance UseSpacing Attribute where
  spacing value = Spacing value

instance UseWidth Length Attribute where
  width len = Width len

instance UseHeight Length Attribute where
  height len = Height len

row :: [Attribute] -> [Element] -> Element
row attributes children = pack Row { .. }

useAlignItems :: Alignment -> AttributeFn
useAlignItems value self = do
  let nativeAlignment = alignmentToNative value
  row_align_items self nativeAlignment

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self = do
  row_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useSpacing :: Float -> AttributeFn
useSpacing value self = do
  row_spacing self (CFloat value)

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  row_width self nativeLen

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  row_height self nativeLen
