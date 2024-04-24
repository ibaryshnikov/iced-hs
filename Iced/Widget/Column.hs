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
import Iced.Attribute.Internal
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
  into_element :: Self -> IO ElementPtr

data Column = Column { children :: [Element] }

instance Builder Self where
  build = into_element

instance IntoNative Column Self where
  toNative details = do
    elements <- buildElements details.children []
    let len = fromIntegral $ length elements
    elementsPtr <- newArray elements
    self <- column_with_children len elementsPtr
    free elementsPtr
    pure self

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Spacing value -> useFn column_spacing value
    AddPadding value -> usePadding value
    AlignItems value -> useFn column_align_items value
    Width  len -> useFn column_width  len
    Height len -> useFn column_height len

instance UseAlignment Attribute where
  alignItems = AlignItems

instance UseSpacing Attribute where
  spacing = Spacing

instance PaddingToAttribute Padding Attribute where
  paddingToAttribute = AddPadding

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

column :: [Attribute] -> [Element] -> Element
column attributes children = pack Column { .. } attributes

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  column_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)
