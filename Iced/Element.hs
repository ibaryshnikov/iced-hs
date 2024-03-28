module Iced.Element (
  NativeElement,
  Element(..),
  IntoNative,
  pack,
  elementToNative,
  buildElements,
  applyAttributes,
  ElementPtr,
  toNative,
) where

import Foreign

data NativeElement
type ElementPtr = Ptr NativeElement

class IntoNative widget where
  toNative :: widget -> IO ElementPtr

data Element
  where
  MakeElement :: IntoNative a => a -> Element

pack :: IntoNative widget => widget -> Element
pack = MakeElement

elementToNative :: Element -> IO (ElementPtr)
elementToNative (MakeElement a) = toNative a

buildElements :: [Element] -> [ElementPtr] -> IO ([ElementPtr])
buildElements [] elements = pure elements
buildElements (first:remaining) elements = do
  native <- elementToNative first
  buildElements remaining (elements ++ [native])

applyAttributes :: widget -> [widget -> IO (widget)] -> IO (widget)
applyAttributes widgetPtr [] = pure widgetPtr
applyAttributes widgetPtr (attribute:remaining) = do
  updatedWidget <- attribute widgetPtr
  applyAttributes updatedWidget remaining
