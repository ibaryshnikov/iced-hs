module Main where

import Iced
import Iced.Widget

data Message = Inc | Dec

update :: Int -> Message -> IO (Int)
update value Inc = pure $ value + 1
update value Dec = pure $ value - 1

view :: Int -> IO (Element)
view value = do
  buttonInc <- button "Inc" Inc
  label <- text $ show value
  buttonDec <- button "Dec" Dec
  column [buttonInc, label, buttonDec]

main :: IO ()
main = Iced.run "Counter" 0 update view
