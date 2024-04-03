{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Row (
  row,
  padding,
  spacing,
) where

import Foreign
import Foreign.C.Types

import Iced.Element

data NativeRow
type SelfPtr = Ptr NativeRow
type Attribute = SelfPtr -> IO SelfPtr

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_row"
--  new_row :: IO (SelfPtr)

-- column top right bottom left
foreign import ccall safe "row_padding"
  row_padding :: SelfPtr -> CFloat -> CFloat -> CFloat -> CFloat -> IO (SelfPtr)

-- row value
foreign import ccall safe "row_spacing"
  row_spacing :: SelfPtr -> CFloat -> IO (SelfPtr)

foreign import ccall safe "row_with_children"
  row_with_children :: CInt -> Ptr ElementPtr -> IO (SelfPtr)

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "row_extend"
--  row_extend :: SelfPtr -> CInt -> Ptr ElementPtr -> IO (SelfPtr)

foreign import ccall safe "row_into_element"
  row_into_element :: SelfPtr -> IO (ElementPtr)

data Row = Row { attributes :: [Attribute], children :: [Element] }

instance IntoNative Row where
  toNative details = do
    elements <- buildElements details.children []
    let len = (length elements)
    elementsPtr <- newArray elements
    selfPtr <- row_with_children (fromIntegral len) elementsPtr
    free elementsPtr
    updatedSelf <- applyAttributes selfPtr details.attributes
    row_into_element updatedSelf

row :: [Attribute] -> [Element] -> Element
row attributes children = pack Row { .. }

padding :: Float -> Attribute
padding value selfPtr = do
  row_padding selfPtr (CFloat value) (CFloat value) (CFloat value) (CFloat value)

spacing :: Float -> Attribute
spacing value selfPtr = do
  row_spacing selfPtr (CFloat value)
