module Iced.Attribute.Style where

class UseStyle value attribute where
  style :: value -> attribute
