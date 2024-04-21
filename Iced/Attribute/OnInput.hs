module Iced.Attribute.OnInput where

class UseOnInput cb a where
  onInput :: cb -> a
