# To Do

This is just a collection of notes. It may be out of date, or make no sense.

## Now-ish

### Program semantics

- Run within the same directory as the _bake.hs_ file.

### Inputs

- Arguments as inputs.
- Environment variables.
  - The environment should be otherwise sanitized.
- Define extra inputs which cannot be picked up automatically.
- Allow for globbing files.
  - If a file is removed from the glob, the input has changed; rebuild.
  - When a glob is specified as an input and outputs are re-built, delete any extra outputs that are no longer necessary.

### Outputs

I would like to support at least:

- [x] files
- [x] arbitrary shell commands
- [ ] HTTP(S)
- [ ] spinning up a process
- [ ] Docker images
- [ ] Docker containers
- [ ] Nix build
- [ ] Nix shell

You should also be able to use an output _temporarily_ in producing another output; for example:

- a free port
- a temporary directory
- a Nix environment
- a running process
- a Docker container

Other things to do:

- Validate names.
- Verify that there are no name collisions.

### Shell

- Add `cd`.
- Run commands on each side of a pipe in parallel.
- Stream data through pipes, instead of collecting it.

### Repetition

- Cache input hashes when running.
- If input hashes haven't changed, don't bother regenerating the output.
  - Assuming it exists.
- Cache `exec` output and simply print it when re-running a target.
- Model pure and impure shell scripts; always re-run the latter.
- Add a `watch` command that watches for changes on the file system and re-runs accordingly.

### Logging

- Log targets as they're invoked.
- Optionally log the plan, before starting.
- Debug logging so we can figure out what goes wrong.
- Log everything in:
  - user-friendly messages (by default)
  - JSON (including lots of extra detail)

## In the far future

- Run the shell standalone.
- Wrap a (simpler) language around the core which doesn't require GHC to invoke it.
  - Preserve type safety.
