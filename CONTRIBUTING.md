# CONTRIBUTING

Thank you for your interest in `iced-hs`!

## Toolchains
### Rust

* `rustc` 1.84

### Haskell

* `GHC` >=9.6.6
* `cabal-install` >=3.14.1.1
* `fourmolu` 0.18.0.0
* `cabal-gild` 1.5.0.1

## Code Style

We use automated formatting. Run the following command to style both rust & haskell codebases:

```bash
make style
```

To selectively format each codebase, you can run:

```bash
make style-hs # For Haskell
```

or 
```bash
make style-rs # For Rust
```

## Pull Requests

* Add a changelog entry when submitting a PR.
