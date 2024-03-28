{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Row (row) where

import Foreign
import Foreign.C.Types

import Iced.Element

data NativeRow
type RowPtr = Ptr NativeRow

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_row"
--  new_row :: IO (RowPtr)

foreign import ccall safe "row_with_children"
  row_with_children :: CInt -> Ptr ElementPtr -> IO (RowPtr)

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "row_extend"
--  row_extend :: RowPtr -> CInt -> Ptr ElementPtr -> IO (RowPtr)

foreign import ccall safe "row_into_element"
  row_into_element :: RowPtr -> IO (ElementPtr)

data Row = Row { children :: [Element], attributes :: [RowPtr -> IO RowPtr] }

instance IntoNative Row where
  toNative details = do
    elements <- buildElements details.children []
    let len = (length elements)
    elementsPtr <- newArray elements
    rowPtr <- row_with_children (fromIntegral len) elementsPtr
    free elementsPtr
    updatedRow <- applyAttributes rowPtr details.attributes
    row_into_element updatedRow

row :: [RowPtr -> IO RowPtr] -> [Element] -> Element
row attributes children = pack Row { children = children, attributes = attributes }
