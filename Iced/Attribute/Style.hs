module Iced.Attribute.Style where

import Iced.Color

class UseStyle value attribute where
  style :: value -> attribute

class UseBackground attribute where
  background :: Color -> attribute

class UseBorder attribute where
  -- color width radius
  border :: Color -> Float -> Float -> attribute

class UseIconColor attribute where
  iconColor :: Color -> attribute

class UsePlaceholderColor attribute where
  placeholderColor :: Color -> attribute

class UseTextColor attribute where
  textColor :: Color -> attribute

class UseSelectionColor attribute where
  selectionColor :: Color -> attribute
