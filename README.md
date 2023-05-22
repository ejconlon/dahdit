# dahdit

Binary parsing and serialization with integrated size.

(dahs and dits are signal durations in Morse code)

This differs from `binary` in a few ways:

* It supports parsing from (or rendering into) pinned or unpinned memory.
* It uses `ByteArray` internally to avoid generating new or retaining references to old pinned memory.
* Examining `Put` can yield required byte size without serializing.
* Derivation uses `via` and not `anyclass` for better control of derived instances.
* Types implementing `Binary` can implement `byteSized` for size calculation (or it will reflect on `put`).
* Known-statically-sized types can implement `StaticByteSized` for size speedups.
* Provides several additional types representing fixed-length strings or sequences (using `TypeLits`).

## Static sizes

You may find it necessary to discharge simple `KnownNat` constraints with a plugin like `ghc-typelits-knownnat`.
Add that package to your dependencies and add this to the top of the necessary files:

    {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

