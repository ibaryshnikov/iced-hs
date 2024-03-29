# iced-hs

Haskell wrapper for [iced](https://github.com/iced-rs/iced) gui library.
Very experimental, an early proof of concept.

To learn more about iced check [official website](https://iced.rs) and the [book](https://book.iced.rs)


## Usage

First, build rust crate to produce `libiced_hs.a`

```bash
./build_rust.sh
```

then pass it to ghc

```bash
ghc -ipath/to/this/repo path/to/libiced_hs.a main.hs
```


## Platform support

Desktop - Linux, Windows, Mac. Supported targets are the same
as for iced except the web target, since it's tricky to link
wasm produced by different compilers. With some effort it
may be possible, just not out of the box.


## Example

Check [examples](./examples) for more

```haskell
import Iced
import Iced.Widget

data Message = Inc | Dec

update :: Int -> Message -> Int
update value Inc = value + 1
update value Dec = value - 1

view :: Int -> Element
view value = column [] [
    button [onClick Inc] "Inc",
    text [] value,
    button [onClick Dec] "Dec"
  ]

main :: IO ()
main = Iced.run "Counter" 0 update view
```


## Roadmap

 - [ ] Add more widgets
   - [x] text
   - [x] textInput
   - [ ] textEditor
   - [x] button
   - [x] checkbox
   - [x] column
   - [x] row
   - [x] horizontalSpace
   - [x] verticalSpace
 - [ ] Styles and themes
