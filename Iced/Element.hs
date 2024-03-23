module Iced.Element (Element) where

import Foreign

data NativeElement

type Element = Ptr NativeElement
