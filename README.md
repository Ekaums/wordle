# Wordle

Wordle in the terminal, built in Haskell.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (9.0+)
- [Cabal](https://www.haskell.org/cabal/)

Or install both via [GHCup](https://www.haskell.org/ghcup/).

## Run

```bash
cabal build
cabal run
```

## Features

- Random 5-letter word each time
- Word validation against real English words
- Emoji feedback (🟩 🟨 ⬛)
- 5 guesses to win
