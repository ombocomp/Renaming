Renaming
=======

Provides a mechanism to perform structured, bulk file renaming.

Control.Monad.FunctionGraph
---------------------------

The core is `Control.Monad.FunctionGraph`, which allows the specification of a DAG of functions through which data is sent, in a manner quite similar to (Kleisli) arrows. Data can be sent along edges, split & merged; computations can be conditional and can result in local or global failure.
