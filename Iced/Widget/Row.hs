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
  into_element :: Self -> IO ElementPtr

data Row = Row { attributes :: [Attribute], children :: [Element] }

instance IntoNative Row where
  toNative details = do
    elements <- buildElements details.children []
    let len = fromIntegral $ length elements
    elementsPtr <- newArray elements
    self <- row_with_children len elementsPtr
    free elementsPtr
    applyAttributes details.attributes self
      >>= into_element

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Spacing value -> useSpacing value
    AddPadding value -> usePadding value
    AlignItems value -> useAlignItems value
    Width len -> useWidth len
    Height len -> useHeight len

instance UseAlignment Attribute where
  alignItems = AlignItems

instance UsePadding Attribute where
  padding v = AddPadding $ Padding v v v v

instance PaddingToAttribute Attribute where
  paddingToAttribute = AddPadding

instance UseSpacing Attribute where
  spacing = Spacing

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

row :: [Attribute] -> [Element] -> Element
row attributes children = pack Row { .. }

useAlignItems :: Alignment -> AttributeFn
useAlignItems value self = row_align_items self $ alignmentToNative value

usePadding :: Padding -> AttributeFn
usePadding Padding { .. } self =
  row_padding self (CFloat top) (CFloat right) (CFloat bottom) (CFloat left)

useSpacing :: Float -> AttributeFn
useSpacing value self = row_spacing self (CFloat value)

useWidth :: Length -> AttributeFn
useWidth len self = row_width self $ lengthToNative len

useHeight :: Length -> AttributeFn
useHeight len self = row_height self $ lengthToNative len
