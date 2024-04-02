{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Column (
  column,
  alignItems,
  padding,
) where

import Foreign
import Foreign.C.Types

import Iced.Alignment (Alignment)
import Iced.AlignmentFFI
import Iced.Element

data NativeColumn
type SelfPtr = Ptr NativeColumn
type Attribute = SelfPtr -> IO SelfPtr

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_column"
--  new_column :: IO (SelfPtr)

foreign import ccall safe "column_align_items"
  column_align_items :: SelfPtr -> AlignmentPtr -> IO (SelfPtr)

-- column top right bottom left
foreign import ccall safe "column_padding"
  column_padding :: SelfPtr -> CFloat -> CFloat -> CFloat -> CFloat -> IO (SelfPtr)

foreign import ccall safe "column_with_children"
  column_with_children :: CInt -> Ptr ElementPtr -> IO (SelfPtr)

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "column_extend"
--  column_extend :: SelfPtr -> CInt -> Ptr ElementPtr -> IO (SelfPtr)

foreign import ccall safe "column_into_element"
  column_into_element :: SelfPtr -> IO (ElementPtr)

data Column = Column { attributes :: [Attribute], children :: [Element] }

instance IntoNative Column where
  toNative details = do
    elements <- buildElements details.children []
    let len = (length elements)
    elementsPtr <- newArray elements
    selfPtr <- column_with_children (fromIntegral len) elementsPtr
    free elementsPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    column_into_element updatedSelf

column :: [Attribute] -> [Element] -> Element
column attributes children = pack Column { .. }

padding :: Float -> Attribute
padding value selfPtr = do
  column_padding selfPtr (CFloat value) (CFloat value) (CFloat value) (CFloat value)

alignItems :: Alignment -> Attribute
alignItems value selfPtr = do
  let nativeAlignment = alignmentToNative value
  column_align_items selfPtr nativeAlignment
