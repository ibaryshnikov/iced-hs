{-# LANGUAGE FunctionalDependencies #-}

module Iced.Attribute.OnInput where

class UseOnInput callback attribute | attribute -> callback where
  onInput :: callback -> attribute
