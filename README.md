Minimal file and directory watching in Erlang using inotify

## Building

```
rebar3 compile
```

## Usage

### `filewatch:start(Pid, Pairs = [{File, Term} ...]) -> {ok, Handle} | {error, _}`
This is an application built around an Erlang port driver that interacts with
inotify. You feed it a list of `{File, Term}` pairs, and when inotify detects
events occurring at `File`, filewatch sends `Term` to `Pid`.

## How it works
An inotify watch descriptor is opened for every unique directory in
`Pairs`. When events for a particular watch descriptor are received from the
Port Driver, they are filtered to only include the files specified in
`Paths`. Watch descriptors are opened on directories rather than individual
files to prevent having to reopen watch descriptors for files that get replaced.

## TODO

- Add cooldown timeout in Erlang

- Add (more) tests

- Add target for afl-fuzz
