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

data Model = Model {
    value :: Int
}

data Message = Inc | Dec

update :: Model -> Message -> IO (Model)
update model message = do
  return $ case message of
    Inc -> Model { value = model.value + 1 }
    Dec -> Model { value = model.value - 1 }

view :: Model -> IO (Element)
view model = do
  button_inc <- button "Inc" Inc
  label <- text $ "Counter value " ++ show model.value
  button_dec <- button "Dec" Dec
  column [button_inc, label, button_dec]

main :: IO ()
main = Iced.run "Counter" Model { value = 0 } update view
```

## Roadmap

 - [ ] Add more widgets
   - [x] text
   - [x] text_input
   - [x] button
   - [x] column
   - [x] row
