{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Widget

data Message = Inc | Dec

update :: Int -> Message -> IO (Int)
update value message = do
  return $ case message of
    Inc -> value + 1
    Dec -> value - 1

view :: Int -> IO(Element)
view value = do
  button_inc <- button "Inc" Inc
  label <- text $ show value
  button_dec <- button "Dec" Dec
  column $ [button_inc, label, button_dec]

main :: IO ()
main = Iced.run "Counter" 0 update view
