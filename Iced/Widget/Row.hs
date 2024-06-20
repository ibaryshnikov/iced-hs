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
import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Attribute.Spacing
import Iced.Attribute.Padding
import Iced.Attribute.PaddingFFI
import Iced.Element

data NativeRow
type Self = Ptr NativeRow

data Attribute
  = Spacing Float
  | AddPadding Padding
  | AlignItems Alignment
  | Width Length
  | Height Length

-- this function is for future use, commented to hide warnings
--foreign import ccall "new_row"
--  new_row :: IO Self

foreign import ccall "row_align_items"
  row_align_items :: Self -> AlignmentPtr -> IO Self

-- row padding
foreign import ccall "row_padding"
  row_padding :: Self -> PaddingPtr -> IO Self

-- row value
foreign import ccall "row_spacing"
  row_spacing :: Self -> CFloat -> IO Self

foreign import ccall "row_with_children"
  row_with_children :: CUInt -> Ptr ElementPtr -> IO Self

-- this function is for future use, commented to hide warnings
--foreign import ccall "row_extend"
--  row_extend :: Self -> CUInt -> Ptr ElementPtr -> IO Self

foreign import ccall "row_width"
  row_width :: Self -> LengthPtr -> IO Self

foreign import ccall "row_height"
  row_height :: Self -> LengthPtr -> IO Self

foreign import ccall "row_into_element"
  into_element :: Self -> IO ElementPtr

data Row = Row { children :: [Element] }

instance Builder Self where
  build = into_element

instance IntoNative Row Self where
  toNative details = do
    elements <- buildElements details.children []
    let len = fromIntegral $ length elements
    elementsPtr <- newArray elements
    self <- row_with_children len elementsPtr
    free elementsPtr
    pure self

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Spacing value -> useFn row_spacing value
    AddPadding value -> useFnIO row_padding value
    AlignItems value -> useFnIO row_align_items value
    Width  len -> useFnIO row_width  len
    Height len -> useFnIO row_height len

instance UseAlignment Attribute where
  alignItems = AlignItems

instance UsePadding Attribute where
  padding = AddPadding . paddingFromOne

instance UsePadding2 Attribute where
  padding2 a b = AddPadding $ paddingFromTwo a b

instance UsePadding4 Attribute where
  padding4 top right bottom left = AddPadding Padding { .. }

instance UseSpacing Attribute where
  spacing = Spacing

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

row :: [Attribute] -> [Element] -> Element
row attributes children = pack Row { .. } attributes
