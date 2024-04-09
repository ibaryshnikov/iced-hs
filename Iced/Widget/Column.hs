{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Column (
  column,
) where

import Foreign
import Foreign.C.Types

import Iced.Alignment
import Iced.AlignmentFFI
import Iced.Element
import Iced.Length
import Iced.LengthFFI
import Iced.Padding
import Iced.Spacing

data NativeColumn
type SelfPtr = Ptr NativeColumn
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute = Spacing Float | AddPadding Padding | AlignItems Alignment
 | Width Length | Height Length

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_column"
--  new_column :: IO (SelfPtr)

foreign import ccall safe "column_align_items"
  column_align_items :: SelfPtr -> AlignmentPtr -> IO (SelfPtr)

-- column top right bottom left
foreign import ccall safe "column_padding"
  column_padding :: SelfPtr -> CFloat -> CFloat -> CFloat -> CFloat -> IO (SelfPtr)

-- column value
foreign import ccall safe "column_spacing"
  column_spacing :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "column_with_children"
  column_with_children :: CUInt -> Ptr ElementPtr -> IO (SelfPtr)

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "column_extend"
--  column_extend :: SelfPtr -> CUInt -> Ptr ElementPtr -> IO (SelfPtr)

foreign import ccall safe "column_width"
  column_width :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "column_height"
  column_height :: SelfPtr -> LengthPtr -> IO (SelfPtr)

foreign import ccall safe "column_into_element"
  column_into_element :: SelfPtr -> IO (ElementPtr)

data Column = Column { attributes :: [Attribute], children :: [Element] }

instance IntoNative Column where
  toNative details = do
    elements <- buildElements details.children []
    let len = fromIntegral $ length elements
    elementsPtr <- newArray elements
    selfPtr <- column_with_children len elementsPtr
    free elementsPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    column_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      Spacing value -> useSpacing value selfPtr
      AddPadding value -> usePadding value selfPtr
      AlignItems value -> useAlignItems value selfPtr
      Width len -> useWidth len selfPtr
      Height len -> useHeight len selfPtr

instance UseAlignment Attribute where
  alignItems value = AlignItems value

instance UseSpacing Attribute where
  spacing value = Spacing value

instance UsePadding Attribute where
  paddingToAttribute value = AddPadding value

instance UseWidth Attribute where
  width len = Width len

instance UseHeight Attribute where
  height len = Height len

column :: [Attribute] -> [Element] -> Element
column attributes children = pack Column { .. }

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } selfPtr = do
  column_padding selfPtr (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useSpacing :: Float -> AttributeFn
useSpacing value selfPtr = do
  column_spacing selfPtr (CFloat value)

useAlignItems :: Alignment -> AttributeFn
useAlignItems value selfPtr = do
  let nativeAlignment = alignmentToNative value
  column_align_items selfPtr nativeAlignment

useWidth :: Length -> AttributeFn
useWidth len selfPtr = do
  let nativeLen = lengthToNative len
  column_width selfPtr nativeLen

useHeight :: Length -> AttributeFn
useHeight len selfPtr = do
  let nativeLen = lengthToNative len
  column_height selfPtr nativeLen
