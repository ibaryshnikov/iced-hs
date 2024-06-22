# iced-hs

Haskell wrapper for [iced](https://github.com/iced-rs/iced) gui library.
Very experimental, check Roadmap to see progress.

To learn more about iced check [official website](https://iced.rs) and the [book](https://book.iced.rs)

The api is inspired by [Elm](https://elm-lang.org/) and [elm-ui](https://github.com/mdgriffith/elm-ui).
Canvas is inspired by [elm-canvas](https://github.com/joakin/elm-canvas)

Note: temporarily switched to a fork to provide access to KeyCode from winit.
Currently iced provides only logical key, check [this pr](https://github.com/iced-rs/iced/pull/2169)
for more details. Eventually physical key will be added back to iced.
When that happens, the iced dependency will be changed back to the original crate.
Also, was using latest upstream to get unreleased styles api. Will use the
version from crates.io when all the necessary changes are published.


## Platform support

Desktop - Windows, macOS, Linux. Supported targets are the same
as for iced except the web target, since it's tricky to link
wasm produced by different compilers. With some effort it
may be possible, just not out of the box.


## Example

Check [examples](examples) for more

```haskell
import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget

data Message = Inc | Dec

update :: Int -> Message -> Int
update value Inc = value + 1
update value Dec = value - 1

view :: Int -> Element
view value =
  center [] $
  column [alignItems Center, spacing 10] [
    button [onPress Inc] "Increment",
    text [size 50] $ show value,
    button [onPress Dec] "Decrement"
  ]

main :: IO ()
main = Iced.run [theme GruvboxLight] "Counter" 0 update view
```

![Counter preview](examples/counter/counter.png)


## Preview

From [themes](examples/themes) example:

![Themes preview](examples/themes/themes.png)


## Usage

First, build rust crate to produce `libiced_hs.a`

```bash
./build_rust.sh
```

then pass it to ghc

```bash
ghc -ipath/to/this/repo path/to/libiced_hs.a main.hs
```


## Internals

- [wgpu](https://github.com/gfx-rs/wgpu) - graphics
- [winit](https://github.com/rust-windowing/winit) - windowing
- [tokio](https://github.com/tokio-rs/tokio) - runtime, though other runtimes
  are available through features in original iced crate


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
 - [x] Themes
 - [ ] Canvas api
   - [x] canvas widget
   - [x] fill path
   - [x] stroke path
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
   - [ ] Keyboard
     - [x] onKeyPress
     - [x] onKeyRelease
     - [ ] PhysicalKey
       - [x] KeyCode
       - [ ] NativeKeyCode
     - [ ] LogicalKey
       - [x] Named
       - [ ] Character
   - [ ] custom subscriptions
   - [ ] other events
 - [x] Command api
   - [x] `Command.perform` for Rust `Future`
   - [x] `Command.performBlocking` for blocking tasks
 - [ ] Add all attributes from iced for each widget
 - [ ] Styles for widgets
   - [x] button
   - [x] checkobx
   - [x] container
   - [x] text
   - [x] textInput
   - [x] textEditor
   - [ ] ...
