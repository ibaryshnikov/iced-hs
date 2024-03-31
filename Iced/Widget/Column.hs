{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Column (column) where

import Foreign
import Foreign.C.Types

import Iced.Element

data NativeColumn
type SelfPtr = Ptr NativeColumn
type Attribute = SelfPtr -> IO SelfPtr

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "new_column"
--  new_column :: IO (SelfPtr)

foreign import ccall safe "column_with_children"
  column_with_children :: CInt -> Ptr ElementPtr -> IO (SelfPtr)

-- this function is for future use, commented to hide warnings
--foreign import ccall safe "column_extend"
--  column_extend :: SelfPtr -> CInt -> Ptr ElementPtr -> IO (SelfPtr)

foreign import ccall safe "column_into_element"
  column_into_element :: SelfPtr -> IO (ElementPtr)

data Column = Column { children :: [Element], attributes :: [Attribute] }

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
column attributes children = pack Column { children = children, attributes = attributes }
