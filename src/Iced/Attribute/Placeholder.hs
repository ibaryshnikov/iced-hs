module Iced.Attribute.Placeholder where

class UsePlaceholder attribute where
  placeholder :: String -> attribute
