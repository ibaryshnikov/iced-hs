module Iced.Widget.Space (
  space,
  horizontal,
  vertical,
) where

import Foreign

import Iced.Attribute.Internal
import Iced.Attribute.LengthFFI
import Iced.Element

data NativeSpace
type Self = Ptr NativeSpace

data Attribute
  = Width Length
  | Height Length

foreign import ccall "space_new"
  space_new :: IO Self

foreign import ccall "space_horizontal"
  space_horizontal :: IO Self

foreign import ccall "space_vertical"
  space_vertical :: IO Self

foreign import ccall "space_width"
  space_width :: Self -> LengthPtr -> IO Self

foreign import ccall "space_height"
  space_height :: Self -> LengthPtr -> IO Self

foreign import ccall "space_into_element"
  into_element :: Self -> IO ElementPtr

data Space = Space | Horizontal | Vertical

instance Builder Self where
  build = into_element

instance IntoNative Space Self where
  toNative details = do
    makeSpace details

makeSpace :: Space -> IO Self
makeSpace kind = case kind of
  Space -> space_new
  Horizontal -> space_horizontal
  Vertical -> space_vertical

instance UseAttribute Self Attribute where
  useAttribute attribute = case attribute of
    Width len -> useFnIO space_width len
    Height len -> useFnIO space_height len

instance UseWidth Length Attribute where
  width = Width

instance UseHeight Length Attribute where
  height = Height

space :: [Attribute] -> Element
space = pack Space

horizontal :: [Attribute] -> Element
horizontal = pack Horizontal

vertical :: [Attribute] -> Element
vertical = pack Vertical
