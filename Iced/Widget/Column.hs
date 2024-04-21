{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Column (
  column,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Alignment
import Iced.Attribute.AlignmentFFI
import Iced.Attribute.Length
import Iced.Attribute.LengthFFI
import Iced.Attribute.Padding
import Iced.Attribute.Spacing
import Iced.Element

data NativeColumn
type Self = Ptr NativeColumn
type AttributeFn = Self -> IO Self

data Attribute
  = Spacing Float
  | AddPadding Padding
  | AlignItems Alignment
  | Width Length
  | Height Length

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_column"
--  new_column :: IO Self

foreign import ccall safe "column_align_items"
  column_align_items :: Self -> AlignmentPtr -> IO Self

-- column top right bottom left
foreign import ccall safe "column_padding"
  column_padding :: Self -> CFloat -> CFloat -> CFloat -> CFloat -> IO Self

-- column value
foreign import ccall safe "column_spacing"
  column_spacing :: Self -> CFloat -> IO Self

foreign import ccall safe "column_with_children"
  column_with_children :: CUInt -> Ptr ElementPtr -> IO Self

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "column_extend"
--  column_extend :: Self -> CUInt -> Ptr ElementPtr -> IO Self

foreign import ccall safe "column_width"
  column_width :: Self -> LengthPtr -> IO Self

foreign import ccall safe "column_height"
  column_height :: Self -> LengthPtr -> IO Self

foreign import ccall safe "column_into_element"
  column_into_element :: Self -> IO ElementPtr

data Column = Column { attributes :: [Attribute], children :: [Element] }

instance IntoNative Column where
  toNative details = do
    elements <- buildElements details.children []
    let len = fromIntegral $ length elements
    elementsPtr <- newArray elements
    self <- column_with_children len elementsPtr
    free elementsPtr
    updatedSelf <- applyAttributes self details.attributes
    column_into_element updatedSelf

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

instance UseSpacing Attribute where
  spacing value = Spacing value

instance UsePadding Attribute where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute Attribute where
  paddingToAttribute value = AddPadding value

instance UseWidth Length Attribute where
  width len = Width len

instance UseHeight Length Attribute where
  height len = Height len

column :: [Attribute] -> [Element] -> Element
column attributes children = pack Column { .. }

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self = do
  column_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useSpacing :: Float -> AttributeFn
useSpacing value self = do
  column_spacing self (CFloat value)

useAlignItems :: Alignment -> AttributeFn
useAlignItems value self = do
  let nativeAlignment = alignmentToNative value
  column_align_items self nativeAlignment

useWidth :: Length -> AttributeFn
useWidth len self = do
  let nativeLen = lengthToNative len
  column_width self nativeLen

useHeight :: Length -> AttributeFn
useHeight len self = do
  let nativeLen = lengthToNative len
  column_height self nativeLen
