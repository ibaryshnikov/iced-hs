{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Column (
  column,
) where

import Foreign
import Foreign.C.Types

import Iced.Attribute.Alignment
import Iced.Attribute.AlignmentFFI
import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.PaddingFFI
import Iced.Attribute.Spacing
import Iced.Element

data NativeColumn
type Self = Ptr NativeColumn

data Attribute
  = Spacing Float
  | AddPadding Padding
  | AlignX Alignment
  | Width Length
  | Height Length

-- this function is for future use, commented to hide warnings
-- foreign import ccall "new_column"
--  new_column :: IO Self

foreign import ccall "column_align_x"
  column_align_x :: Self -> AlignmentPtr -> IO Self

-- column padding
foreign import ccall "column_padding"
  column_padding :: Self -> PaddingPtr -> IO Self

-- column value
foreign import ccall "column_spacing"
  column_spacing :: Self -> CFloat -> IO Self

foreign import ccall "column_with_children"
  column_with_children :: CUInt -> Ptr ElementPtr -> IO Self

-- this function is for future use, commented to hide warnings
-- foreign import ccall "column_extend"
--  column_extend :: Self -> CUInt -> Ptr ElementPtr -> IO Self

foreign import ccall "column_width"
  column_width :: Self -> LengthPtr -> IO Self

foreign import ccall "column_height"
  column_height :: Self -> LengthPtr -> IO Self

foreign import ccall "column_into_element"
  into_element :: Self -> IO ElementPtr

data Column = Column {children :: [Element]}

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
    AddPadding value -> useFnIO column_padding value
    AlignX value -> useFnIO column_align_x value
    Width len -> useFnIO column_width len
    Height len -> useFnIO column_height len

instance UseAlignX Attribute where
  alignX = AlignX

instance UseSpacing Attribute where
  spacing = Spacing

instance UsePadding Attribute where
  padding = AddPadding . paddingFromOne

instance UsePadding2 Attribute where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 Attribute where
  padding4 top right bottom left = AddPadding Padding{..}

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

column :: [Attribute] -> [Element] -> Element
column attributes children = pack Column{..} attributes
