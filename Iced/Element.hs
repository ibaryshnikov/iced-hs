{-# LANGUAGE FunctionalDependencies #-}

module Iced.Element (
  NativeElement,
  Element,
  IntoNative,
  toNative,
  UseAttribute,
  useAttribute,
  Builder,
  build,
  pack,
  packSimple,
  elementToNative,
  buildElements,
  ElementPtr,
) where

import Control.Monad
import Foreign

import Iced.Attribute.Internal

data NativeElement
type ElementPtr = Ptr NativeElement

class Builder native where
  build :: native -> IO ElementPtr

class IntoNative widget native | widget -> native where
  toNative :: widget -> IO native

data Element where
  Simple :: IntoNative widget ElementPtr
         => widget -> Element
  Custom :: (IntoNative widget native, UseAttribute native attribute, Builder native)
         => widget -> [attribute] -> Element

pack :: (IntoNative widget native, UseAttribute native attribute, Builder native)
     => widget -> [attribute] -> Element
pack = Custom

packSimple :: IntoNative widget ElementPtr => widget -> Element
packSimple = Simple

elementToNative :: Element -> IO ElementPtr
elementToNative (Simple widget) = toNative widget
elementToNative (Custom widget attributes) =
  toNative widget
    >>= applyAttributes attributes
    >>= build

buildElements :: [Element] -> [ElementPtr] -> IO [ElementPtr]
buildElements [] elements = pure elements
buildElements (first:remaining) elements = do
  native <- elementToNative first
  buildElements remaining (elements ++ [native])

applyAttributes :: UseAttribute widget attribute => [attribute] -> widget -> IO widget
applyAttributes [] = pure
applyAttributes (attribute:remaining) =
  useAttribute attribute >=> applyAttributes remaining
