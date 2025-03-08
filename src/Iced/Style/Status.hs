{-# LANGUAGE FunctionalDependencies #-}

module Iced.Style.Status where

class UseActive value attribute | attribute -> value where
  active :: value -> attribute

class UseHovered value attribute | attribute -> value where
  hovered :: value -> attribute

class UseFocused value attribute | attribute -> value where
  focused :: value -> attribute

class UseOpened value attribute | attribute -> value where
  opened :: value -> attribute

class UsePressed value attribute | attribute -> value where
  pressed :: value -> attribute

class UseDisabled value attribute | attribute -> value where
  disabled :: value -> attribute
