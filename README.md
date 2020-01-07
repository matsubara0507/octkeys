# octkeys

Manage ssh authorized keys by GitHub Account

## Requirement

Haskell Stack or Docker

## Usage

write config YAML file (ref. [example/.octkeys.yaml](example/.octkeys.yaml)).

```
$ octkeys --help
octkeys [options] [input-file]
  -h       --help         Show this help text
           --version      Show version
  -v       --verbose      Enable verbose mode: verbosity level "debug"
  -F PATH  --dotssh=PATH  ssh config directory path instead of ~/.ssh

$ octkeys -F example/.ssh example/.octkeys.yaml
```

## Build

```
$ stack build
```

### Docker

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/octkeys . --build-arg local_bin_path=./bin
```
