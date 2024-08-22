{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.ByteString qualified as ByteString
import Data.Word
import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget
import Iced.Widget.Image qualified as Image

data Source = File | Bytes | Pixels deriving (Show, Read)

data Model = Model {
  source :: Source,
  bytes :: ByteString.ByteString
}

data Message = Selected Source

update :: Message -> Model -> Model
update (Selected source) model = model { source = source }

pixelsRow :: Float -> Float -> [Word8]
pixelsRow y x = [r, g, b, 255]
  where
    y' = 501 - y -- invert y
    q = x * x + y' * y'
    r = round $ 255 * x / 500
    g = round $ 255 * y' / 500
    b = round $ 255 * sin (q / 100)

pixels :: [Word8]
pixels = concat $ pixelsRow <$> [1..500] <*> [1..500]

getImage :: Model -> Element
getImage model = case model.source of
  File -> image attributes "watch_3.png"
  Bytes -> image attributes model.bytes
  Pixels -> Image.fromRgba attributes 500 500 pixels
  where
    attributes = [width (Fixed 500), height (Fixed 500)]

options :: [Source]
options = [File, Bytes, Pixels]

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  column [spacing 20, alignItems Center] [
    pickList [] options (Just model.source) Selected,
    getImage model
  ]

main :: IO ()
main = do
  bytes <- ByteString.readFile "empty.png"
  let model = Model { source = Pixels, bytes = bytes }
  Iced.run [theme Oxocarbon] "Image" model update view
