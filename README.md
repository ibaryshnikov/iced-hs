# iced-hs

Haskell wrapper for [iced](https://github.com/iced-rs/iced) gui library.
Very experimental.

To learn more about iced check [official website](https://iced.rs) and the [book](https://book.iced.rs)

The api is inspired by [Elm](https://elm-lang.org/) and [elm-ui](https://github.com/mdgriffith/elm-ui).
Canvas is inspired by [elm-canvas](https://github.com/joakin/elm-canvas)


## Platform support

Desktop - Windows, macOS, Linux. Supported targets are the same
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
    button [onPress Inc] "Inc",
    text [] $ show value,
    button [onPress Dec] "Dec"
  ]

main :: IO ()
main = Iced.run [] "Counter" 0 update view
```


## Usage

First, build rust crate to produce `libiced_hs.a`

```bash
./build_rust.sh
```

then pass it to ghc

```bash
ghc -ipath/to/this/repo path/to/libiced_hs.a main.hs
```


## Status

This is a research project. The api may change often.
Not recommended for use in production. But may be a good
option if you are learning Haskell or need a simple
way to build a ui or display some data.


## Roadmap

 - [ ] Widgets
   - [x] button
   - [x] checkbox
   - [x] column
   - [x] comboBox
   - [x] container
   - [ ] image
   - [ ] keyedColumn
   - [ ] mouseArea
   - [ ] paneGrid
   - [x] progressBar
   - [x] pickList
   - [ ] qrCode
   - [x] radio
   - [x] responsive
   - [ ] horizontalRule, verticalRule
   - [x] row
   - [x] scrollable
   - [x] slider, verticalSlider
   - [x] horizontalSpace, verticalSpace
   - [ ] svg
   - [x] text
   - [x] textEditor
   - [x] textInput
   - [x] toggler
   - [x] tooltip
 - [ ] Helper functions
   - [ ] lazy
   - [ ] themer
   - [ ] focusNext, focusPrevious
 - [ ] Canvas api
   - [x] canvas widget
   - [x] fill path
   - [ ] stroke path
   - [x] path methods - circle, lineTo, moveTo, rectangle
   - [ ] more path methods
 - [ ] Shader
   - [ ] shader widget
   - [ ] attributes
 - [ ] Multi window
 - [ ] Custom widgets
 - [ ] Subscriptions
   - [x] subscription attribute for Application
   - [x] Time.every subscription
   - [ ] keyboard events
   - [ ] custom subscriptions
   - [ ] other events
 - [x] Command api
   - [x] multiple signatures for `update` function:
     ```haskell
     Model -> Message ->    Model
     Model -> Message -> IO Model
     Model -> Message ->    (Model, Command Message)
     Model -> Message -> IO (Model, Command Message)
     ```
   - [x] `Command.Perform` for Rust `Future`
   - [x] `Command.PerformIO` for blocking tasks
 - [ ] Add all attributes from iced for each widget
 - [ ] theme attribute for Application
 - [ ] Styles for widgets
   - [x] button
   - [ ] ...
