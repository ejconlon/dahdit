# dahdit

Binary parsing with integrated size.

This differs from `binary` in a few ways:

1. It is required that all types implementing `Binary` must also implement `ByteSized`.
2. Known-statically-sized types can implement `StaticByteSized` for size speedups.
3. Examining `Put` can yield required byte size without serializing.
4. It uses `ShortByteString` to avoid pinned memory.
5. It supports parsing `PrimArray` for fast copies of repeated chunks of primitive data.
6. Derivation uses `via` and not `anyclass` for better control of derived instances.
7. Provides several additional types representing fixed-length strings or sequences (using `TypeLits`).

This generally works but should be considered experimental.

(dahs and dits are signal durations in Morse code)
