Minimal file and directory watching in Erlang using inotify

This is an application built around an Erlang port driver that interacts with
inotify.

## Building

```
rebar3 compile
```

## Usage

### `erl-filewatch:start(Self, Paths) -> {ok, Handle} | {error, _}`

Starts watching files/directories in `Paths`. Events that occur are batched until
`CooldownMs` seconds have elapsed without events, or until 12 messages are
collected.

`Paths` is comprised of `{"path/to/file", Term}` or `{"path/to/dir/*", Term}`
pairs. An inotify watch descriptor is opened on each unique directory in
`Paths`, and a mapping of `{Descriptor, Filename}` to `Term` is created. When a
particular watch is triggered, a `{Descriptor, Filename}` pair is sent from the
port driver to Erlang, and is used as a key to find the correct `Term` to send
to `Self`, if any.

## FAQ

### Why not open watch descriptors on individual files?

The specific intended use case for this library is to trigger a response when a
particular file is replaced. If an inotify watch descriptor is open on a file
that gets replaced, no further events can be read from that descriptor. One
option is to reopen a watch descriptor whenever a file is replaced, but watching
directories and filtering in Erlang seems simpler.

## TODO

- Add cooldown timeout in Erlang

- Add tests

- Add target for afl-fuzz
