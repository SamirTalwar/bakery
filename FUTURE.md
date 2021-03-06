# The Bakery

_The following is a work of fiction, following README-driven development principles._

## Getting Started

Welcome to The Bakery. Let's bake some goodies.

We'll start with a simple example.

```bake
file:./README.md -> remove-vowels -> file:./consonants.md

remove-vowels = process:
  command: sed 's/[aeiou]//'
  stdin: input.stream
  stdout: output.stream
```

This _cookbook_ contains a recipe for baking a file, "consonants.md", from this very README, by running it through a process named "remove-vowels".

Copy it into a file named _default.bake_ and run `bake`. You should see a new file, "consonants.md", which is exactly the same as this one except with the letters "a", "e", "i", "o", and "u" removed.

Run `bake` again. The second time, nothing will happen, because _README.md_ hasn't changed. The Bakery checks both the modification timestamp of the file as well as its contents (using an SHA256 hash) to verify that it needs to do anything. If either changes, it will consider the file to be changed, and so the process will run again.

### Properties

Of course, sometimes you don't want to pipe text through STDIN and STDOUT, but deal with the file some other way. For example, by passing the path to the command.

This is also pretty easy in The Bakery.

```bake
file:./README.md -> remove-vowels -> file:./consonants.md

remove-vowels = process:
  command: sed 's/[aeiou]//' '$[$input.path]' > '$[$output.path]'
  shell: /bin/sh  # or just "sh", or "bash", or…
```

`input` and `output` are rich objects, with properties dependent on the types of the inputs and outputs. In this case, `stream` and `path` are both properties on _files_. If the input was different, the properties would be different—for example, if the input was the output from another process, `stream` would be available (because the process has STDOUT), but `path` would not be.

More information is available in the API documentation.

### Writing to the console

We don't just have to work with files, though. For example, instead of writing to a file, we could write to the console's STDOUT:

```bake
file:./README.md -> remove-vowels -> console:out
```

Or we could read from STDIN:

```bake
console:in -> remove-vowels -> file:./consonants.md
```

Here's an implementation of `cat` in The Bakery:

```bake
console:in -> console:out
```

### Splitting

It's common to want to do two things with a single input. The Bakery will happily handle that with multiple recipes in one cookbook:

```bake
file:./README.md -> remove-vowels -> file:./consonants.md

file:./README.md -> remove-consonants -> file:./vowels.md
```

Running `bake` will only bake the first one. To bake them both, we need to tell it to:

```sh
$ bake file:./consonants.md file:./vowels.md
```

Sometimes we need to split an output from a previous step into two. We can store the intermediate result in a file:

```bake
file:./first-10-lines.md -> remove-vowels -> file:./consonants.md

file:./first-10-lines.md -> remove-consonants -> file:./vowels.md

file:./README.md -> first-10-lines -> file:./first-10-lines.md

...

first-10-lines = process:
  command: head -n 10
  stdin: input.stream
  stdout: output.stream
```

These files can often clutter up the local directory, though, and because they can change, might trigger work when none is necessary. Instead, we can use a `store` to capture intermediate data.

```bake
README-first-10 -> remove-vowels -> file:./consonants.md

README-first-10 -> remove-consonants -> file:./vowels.md

file:./README.md -> first-10-lines -> README-first-10

README-first-10 = store()
```

This also has the advantage of capturing data that isn't easily stored in a file. More on that later.

### Joining

Often, baking an output will require multiple inputs. We can denote these with spaces:

This recipe concatenates two files and prints them to STDOUT:

```bake
file:./consonants.md, file:./vowels.md -> console:out
```

We can use this syntax to add a top-level recipe that relies on all the other recipes, but doesn't do anything with them. This lets us avoid typing the names of recipes on the command line.

```bake
everything = file:./consonants.md, file:./vowels.md
```

Or we could write the results to a file:

```bake
file:./consonants.md, file:./vowels.md -> file:./consonants-and-vowels.md
```

If we wanted to pipe through a process, we could pass the arguments as follows:

```bake
file:./consonants.md, file:./vowels.md -> cat -> file:./consonants-and-vowels.md

cat = process:
  command: cat $[map .path $inputs]
  stdout: .output.stream
```

You saw `input` earlier, but `inputs` is the more general form. `input` is a shorthand for "the only input", and only works if there's just one.

Inputs are ordered, so you don't have to worry about things changing each time you run `bake`.

As an aside, because we are not specifying a shell, The Bakery will split the arguments to the command. This means that because it knows that the `map .path inputs` expression results in a collection of file paths, which will be passed as many arguments. This will work even if one of them contains a space or other separating character.

### Working to schedule

Let's consider another input:

```bake
time:now -> console:out
```

Running `bake` will print the current time to the console. Because the time is always changing, this will re-run every time you `bake`.

If we would like to be less granular, we can construct a new `time`, which accepts a "resolution":

```bake
minutes -> console:out

minutes = time:
  resolution: 1m
```

The first time you run `bake`, it will print the current time. If you run it again immediately, it won't do anything. You'll have to wait until the next minute (or longer) before it will run again.

We can use `time` to force a recipe to always bake, or to bake to a schedule.

`time:minutes` is a built-in, so you can just use it without defining it.

### Splitting, mapping and joining with wildcards

Often, we need to work on many inputs at once. We can use wildcards (or "globs") to do this.

For example, let's say we had a lot of text files, and we needed them all in lowercase. We'd use wildcards to specify them as a collection, and then map over them.

```bake
file:./inputs/*.txt -> each as $item -> lowercase -> file:./outputs/$[$item.basename].txt

lowercase = process:
  command: tr '[:upper:]' '[:lower:]'
  stdin: input.stream
  stdout: output.stream
```

`each` is a recipe that takes a collection of inputs and produces many outputs, one for each item. It allows us to split the collection, and bake an output for each item separately. We name each item `$item`, and then refer to it in the declaration for the final recipe.

Baking all of these would run the whole thing in parallel.

We could also bake a single one by running, for example, `bake file:./outputs/example.txt`.

For more sophisticated wildcards, we can use multiple asterisks:

```bake
file:./inputs/*/*.txt -> lowercase -> file:./outputs/*/*.txt
```

The first input wildcard corresponds to the first output wildcard, and so on. There must be an equal number of asterisks on each side.

Sometimes we really do want to pipe all the files to a single operation. To do this, we just drop the `each`. The following recipe bakes a single file from all input files by concatenating them.

```bake
file:./inputs/*.txt -> file:./output.txt
```

Or we might break the collection into chunks of 10 by using the `chunk` recipe:

```bake
file:./inputs/*.txt -> chunk(10) as $chunk -> file:./output/$[$chunk.index].txt
```

Finally, if the contents of the _inputs_ directory changes, re-running `bake` will re-run the recipe for the changed files. However, it won't delete output files that correspond to deleted input files by default. You can specify that The Bakery should delete "orphaned" outputs by adding a `!` to the start of the recipe name:

```bake
file:./inputs/*.txt -> each as $item -> lowercase -> !file:./outputs/$[$item.basename].txt
```

### Failure and Validation

Processes can _fail_. If a process exits with a non-zero exit code, it will have failed, and the thread of execution will stop.

If this happens, `bake` will report the error and exit with an exit code of `1`.

Here's an example of a cookbook that will always fail:

```bake
always-fails -> file:./irrelevant.txt

always-fails = process:
  command: false
```

It'll result in the error:

```
always-fails :: ERROR: The process `false` exited with a status code of 1.
```

The Bakery is designed to be operated under supervision, so often, the right thing to do is to just tell the user that an error occured. For example, this could fail:

```bake
console:in -> file:./output.txt
```

It might fail because we don't have write access, or because we're out of disk space, or one of hundreds of other reasons. In this circumstance, we probably don't want to try and handle the error. We just want to know about it. And so `bake` will print something like this:

```
file:./output.txt :: ERROR: Could not write to file \"./output.txt\". Access denied.
```

It's on you to figure out why.

Sometimes, however, we have a programmer error, and those, we can also report on.

For example:

```bake
file:./input.txt -> file:./output.txt
```

If _input.txt_ doesn't exist and there's no recipe to make one, we'll get an error, but this time, it won't even try to run. It'll just fail straight away.

```
file:./output.txt :: ERROR: No recipe for `file:./input.txt`.
```

There are occasions, however, where we can do something about the error. For example, here's a more insidious one:

```bake
file:./inpputs/*.txt -> file:./output.txt
```

If there are no input files, the glob will expand to contain no files, and so the output file will be empty. But wait, it's just a typo; we actually meant "inputs", not "inpputs".

Typically, you don't expect zero inputs, so `bake` will generate an error:

```
file:./output.txt :: ERROR: `file:./inpputs/*.txt` resulted in no items.
```

Of course, there are cases where no items is completely valid, so you can mark those explicitly to suppress the error:

```bake
*** file:./inpputs/*.txt -> file:./output.txt
```

Lastly, we might want to verify something more advanced. For example, perhaps we expect there to be at least 2 input files. We can cater for this by adding a specific check:

```bake
file:./inputs/*.txt -> check(.length >= 2) -> file:./output.txt
```

`check` is a Bakery function that, given a validation function, validates an input (or multiple inputs). If everything checks out, it passes its input on as output; if not, it'll error:

```
file:./output.txt :: ERROR: Failed validation.
  Rule: .length >= 2
  Input:
    - file:./inputs/only.txt
```

### Concurrency and parallelism

Let's take this cookbook below:

```bake
everything = file:./consonants.md, file:./vowels.md

stored-first-10-lines -> remove-vowels -> file:./consonants.md

stored-first-10-lines -> remove-consonants -> file:./vowels.md

file:./README.md -> first-10-lines -> stored-first-10-lines

stored-first-10-lines = store()
```

If we run `bake`, it will split, then join again. Because they're independent, there's no need to produce _consonants.md_ and _vowels.md_ serially. We can run them in parallel. So we do.

The Bakery will parallelise as much as it can. This means that outputs can fail (typically because a process fails) and it will still finish the other threads of execution before reporting the failure.

### Repetition

Often we want to re-run a recipe every time one of the inputs changes. For example, we might want to recreate an output file when the requisite input file changes, or re-build a recipe every minute.

This is easy. Just run `bake --watch`. It will watch the inputs and bake outputs as necessary. If something fails, it will report the failure but carry on watching.

For example, here's a silly recipe that will print the current minute:

```bake
time:minutes -> console:out
```

Running `bake --watch` will print the time _every_ minute until the process is stopped (with Ctrl+C).

### Running remotely

`bake` can be configured to run tasks on a third-party server. Instead of running processes locally, it will run them in containers on another machine. Coordination will still be done locally, so if you have long-running processes, you can't put your machine to sleep.

To do this, run The Bakery as a server:

```sh
$ bake --serve
```

You may want to wrap a supervisor such as _supervisord_ or _Docker_ around it to make sure it keeps running. If you like, you can use the official Docker image:

```sh
$ docker run --detach --publish=2253:2253 --restart=unless-stopped bake --serve
```

Then, instruct `bake` to connect to it by setting the `BAKE_HOST` environment variable.

```sh
$ BAKE_HOST=host.for.the.bakery.instance bake
```

You can also set `BAKE_PORT` to run it on a different port.

### What's next?

Here's a secret. `file`, `process`, `console`, `time`, `each`… these are all plugins. They're not core to The Bakery.

There are many more plugins, and it's easy to write your own. If you're reading this and I haven't explained how, it's because this documentation is incomplete. Get in touch and let me know!

## Plugin List

This is not exhaustive; it's just what I think might be useful.

- files
- URLs
- system clock
- environment variables
- running processes
- dependency management
- Java classes
- test results
  - JUnit
  - tap
  - arbitrary program (using the exit code)
- Git repository clones
- Docker images
- Docker containers
- Kubernetes resources
- Ansible modifier (so any Ansible module can be a recipe)

## Other uses

We've talked entirely about "building" in this document, but there are other ways to apply this language (or a variation of it).

We could create a shell that operates in a more _static_ fashion, allowing for more safety and surety when running commands. It would also allow us to easily "dry-run" operations (in other words, predict the output without actually changing anything).

We could create a continuous integration and deployment server that's effectively Watch mode with a nice UI.

Or we could even build [an event sourcing system that knows how to flow events between (serverless?) processes](https://monospacedmonologues.com/2018/07/functional-reactive-serverless-architecture/).
