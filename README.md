# dahdit

Binary parsing and serialization with integrated size.

This differs from `binary` in a few ways:

* It is required that all types implementing `Binary` must also implement `ByteSized`.
* Known-statically-sized types can implement `StaticByteSized` for size speedups.
* Examining `Put` can yield required byte size without serializing.
* It uses `ShortByteString` to avoid pinned memory.
* It supports parsing `PrimArray` for fast copies of repeated chunks of primitive data.
* Derivation uses `via` and not `anyclass` for better control of derived instances.
* Provides several additional types representing fixed-length strings or sequences (using `TypeLits`).

This generally works but should be considered experimental.

(dahs and dits are signal durations in Morse code)
