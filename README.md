# iced-hs

Haskell wrapper for [iced](https://github.com/iced-rs/iced) gui library.
Very experimental, an early proof of concept

## Usage

Currently the app code lives in main.hs
You can modify it and build with these scripts. Also, check the [examples](./examples)

```bash
./build_rust.sh
./build_haskell.sh
```

## Example

```haskell
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
```

## Roadmap

 - [ ] Add more widgets
   - [x] text
   - [x] text_input
   - [x] button
   - [x] checkbox
   - [x] column
   - [x] row
   - [x] horizontal_space
   - [x] vertical_space
 - [ ] Styles and themes
