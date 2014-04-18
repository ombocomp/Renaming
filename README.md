Renaming
=======

Provides a mechanism to perform structured, bulk file renaming. The underlying (included) modules are `Control.Monad.MonadFilter` and `Control.Monad.FunctionGraph`.

Control.Monad.MonadFilter
-------------------------

`MonadFilter` is a subclass of `MonadPlus` which allows one to determine whether a given value `m a` is `mzero`, even without an `Eq a`-context. Its definition is

```haskell
class MonadPlus m => MonadFilter m where
  mfilter :: [m a] -> m [a]
  ismzero :: m a -> m Bool
  ismzero' :: m a -> m (Maybe a)
```

The minimal complete definition implements `mfilter`, which takes a list of monadic values and returns those among them which are **not** mzero - i.e. those which really contain a value.

Example:
```
instance MonadFilter Maybe where
  mfilter = map fromJust . filter isJust = return . catMaybes
```

Note that this is different from `msum` -- in the case of `Maybe`, `msum` only returns the first `Just`-item. `mfilter` returns all of them.

```bash
> mfilter [Nothing, Just 1, Just 2, Nothing] = Just [1,2]
> msum    [Nothing, Just 1, Just 2, Nothing] = Just 1
```

### MonadFilter laws

Instances of `MonadFilter` have to conform to two laws:

1. `mfilter xs = [x | x <- xs, x is not mzero]`
2. `ismzero x iff mfold [x] is null`

Control.Monad.FunctionGraph
---------------------------

`Control.Monad.FunctionGraph` builds upon `MonadFilter` and is the core module in the program. It allows the specification of a DAG of functions through which data is sent, in a manner quite similar to (Kleisli) arrows. Data can be sent along edges, split & merged; computations can be conditional and can result in local or global failure.

### Pipes

Pipes are type synonyms for stateful functions and come in simple and parallel varieties.

A simple pipe renames one file. Its type is
```haskell
type Pipe m a b = a -> m b
```

A parallel pipe (ParPipe) renames a list of files and keeps track of the renaming history, possibly making use of global information. Its type is
```haskell
type ParPipe m a b c = [(a,b)] -> m [(a, m c)]
```

`a` is the type of the original data. `b` is the type of a previous computation. `c` is the result, wrapped in a monad.

#### Applicative instance

TODO
