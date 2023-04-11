# dahdit

Binary parsing and serialization with integrated size.

(dahs and dits are signal durations in Morse code)

This differs from `binary` in a few ways:

* It supports parsing from (or rendering into) pinned or unpinned memory.
* It uses `ByteArray` internally to avoid generating new or retaining references to old pinned memory.
* Examining `Put` can yield required byte size without serializing.
* Derivation uses `via` and not `anyclass` for better control of derived instances.
* Optionally, types implementing `Binary` can also implement or derive `ByteSized` for size calculation.
* Known-statically-sized types can implement `StaticByteSized` for size speedups.
* Provides several additional types representing fixed-length strings or sequences (using `TypeLits`).

## TODO

* Remaining get/put tests
* Get/put with offset
* Mutable get/put
