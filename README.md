Renaming
=======

Provides a mechanism to perform structured, bulk file renaming.

The core is the `Renaming` newtype, which augments a unary function `FilePath -> Filepath` with a history, giving `(FilePath, [a]) -> (FilePath, [a])`. This makes `Renaming` essentially a simpler, more specialized version of the state monad.

`Renaming` provides the following:

* an identity element
* composition (which allows multipe renamings to share information with each other), 
* and guards.

