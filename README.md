# iced-hs

Haskell wrapper for [iced](https://github.com/iced-rs/iced) gui library.
Very experimental, an early proof of concept

## Usage

Currently the app code lives in main.hs
You can modify it and build with these scripts

```bash
./build_rust.sh
./build_haskell.sh
```

## Example

```haskell
data Model = Model {
    value:: Int
}

data Message = Inc | Dec

update:: Model -> Message -> Model
update model message = case message of
  Inc -> Model { value = model.value + 1 }
  Dec -> Model { value = model.value - 1 }

view:: Model -> IO(Element)
view model = do
  button_inc <- button "Inc" Inc
  label <- text $ "Counter value " ++ show model.value
  button_dec <- button "Dec" Dec
  column [button_inc, label, button_dec]

main = Application.run Model { value = 0 } update view
```

## Roadmap

 - [ ] Add more widgets
   - [x] text
   - [x] text_input
   - [x] button
