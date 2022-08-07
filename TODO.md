# To Do

This is just a collection of notes. It may be out of date, or make no sense.

## Now-ish

### Abstract nonsense

- Figure out how to make `Bake` a lawful functor.
- Model the shell as [stream processors][].

### Program semantics

- Run within the same directory as the _bake.hs_ file.
- Invoke `file` tasks with a `file:` prefix.
- Invoke `exec` tasks by name.

### Shell

- Split the shell out into its own package, decoupled from the library.
- Support multi-line shell scripts.
- Add `cd`.

## In the far future

- Run the shell standalone.
- Wrap a (simpler) language around the core which doesn't require GHC to invoke it.
  - Preserve type safety.

[smoke]: https://github.com/SamirTalwar/smoke
[stream processors]: https://github.com/SamirTalwar/stream-processors
