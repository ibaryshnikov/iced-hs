{-# LANGUAGE FunctionalDependencies #-}

module Iced.Attribute.OnPress where

class UseOnPress message attribute | attribute -> message where
  onPress :: message -> attribute
