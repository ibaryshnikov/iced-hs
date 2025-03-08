{-# LANGUAGE FunctionalDependencies #-}

module Iced.Attribute.OnRelease where

class UseOnRelease message attribute | attribute -> message where
  onRelease :: message -> attribute
