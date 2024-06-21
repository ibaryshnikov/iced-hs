-- Attributes shared between widgets and styles

module Iced.Attribute.Shared where

class UseIcon value attribute where
  icon :: value -> attribute

class UsePlaceholder value attribute where
  placeholder :: value -> attribute
