{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Column (column) where

import Foreign
import Foreign.C.Types

import Iced.Element

data NativeColumn
type ColumnPtr = Ptr NativeColumn

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_column"
--  new_column :: IO (ColumnPtr)

foreign import ccall safe "column_with_children"
  column_with_children :: CInt -> Ptr ElementPtr -> IO (ColumnPtr)

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "column_extend"
--  column_extend :: ColumnPtr -> CInt -> Ptr ElementPtr -> IO (ColumnPtr)

foreign import ccall safe "column_into_element"
  column_into_element :: ColumnPtr -> IO (ElementPtr)

data Column = Column { children :: [Element], attributes :: [ColumnPtr -> IO ColumnPtr] }

instance IntoNative Column where
  toNative details = do
    elements <- buildElements details.children []
    let len = (length elements)
    elementsPtr <- newArray elements
    columnPtr <- column_with_children (fromIntegral len) elementsPtr
    free elementsPtr
    updatedColumn <- applyAttributes columnPtr details.attributes
    column_into_element updatedColumn

column :: [ColumnPtr -> IO ColumnPtr] -> [Element] -> Element
column attributes children = pack Column { children = children, attributes = attributes }
