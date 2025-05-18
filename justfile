ghc-options := "-O0 -Werror"

# Things that are quick
auto: format-cabal format-haskell

# Make sure stuff is okay
check packages='all': (build packages) (test packages)

format-cabal:
  just cabal-files | xargs cabal-fmt --inplace

format-haskell:
  just haskell-files | xargs fourmolu --mode inplace

cabal-files:
  find -maxdepth 1 -name '*.cabal'

haskell-files:
  find library examples test -name '*.hs' -print

build packages='all':
  cabal build {{packages}} --ghc-options="{{ghc-options}}"
  cabal build {{packages}} --enable-test --ghc-options="{{ghc-options}}"

test packages='all':
  cabal test {{packages}} --enable-test --ghc-options="{{ghc-options}}"

clean:
  cabal clean
