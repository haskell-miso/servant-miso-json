servant-miso-json
============================

This package provides a `JSON` content type for use with [servant](https://hackage.haskell.org/packages/servant) APIs. This can be used to render [miso](https://github.com/dmjio/miso) `Value` types as JSON.

### Usage

```haskell
import Servant.Miso.JSON (JSON)

data Person
  = Person
  { name :: MisoString
  , age :: Int
  } deriving stock Generic
    deriving anyclass ToJSON

type API = "person" :> Capture "name" MisoString :> Get '[JSON] Person
```

## Build

```shell
cabal build
```

```shell
nix develop --command bash -c 'cabal build'
```

# Develop

```shell
nix develop
```
