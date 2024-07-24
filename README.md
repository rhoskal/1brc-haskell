# The Billion Row Challenge (1BRC)

1️⃣🐝🏎️ [The One Billion Row Challenge](https://github.com/gunnarmorling/1brc) -- A fun exploration of how quickly 1B rows from a text file can be aggregated.

## The problem

**Input**: a 13GB file containing lines of the form `Amsterdam;10.5`.

- First, a city name. There are cities, and each row has name chosen at random from the set. Their lengths are from **3** to **26** bytes.
- Then a temperature, formatted as `-?\d?\d,\d`, i.e. a possibly negative number with one or two integral digits and exactly one decimal. Each temperature is drawn from a normal distribution for each city.

**Output**: a sorted list of cities of the form `{<station name>=<min>/<average>/<max>}`, each formatted with one decimal place.

## Hardware

```sh
system_profiler SPHardwareDataType
```

```
Model Name: MacBook Pro
Chip: Apple M2 Max
Total Number of Cores: 12 (8 performance and 4 efficiency)
Memory: 64 GB
...
```

## Attempts

| Attempt | Approach                                                                                                                                                                                                                               | Execution Time | Diff    | Commit                                                                                             |
| ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------- | ------- | -------------------------------------------------------------------------------------------------- |
| 0       | Naïve Implementation: Read temperatures into a Map of cities. Iterate serially over each key (station name) in Map to and corresponding list of temperaturs to calculate min, max and mean temperatures. Uses non-performant `String`. | 4714.17 sec    |         | [a58c42d](https://github.com/rhoskal/1brc-haskell/commit/a58c42dcb0b2f414fdfbb1a503777dc42ade1fd2) |
| 1       | Parser uses `Text` instead of `String`, read file as `ByteString` instead of `String`. Store temperatures as `IntX` instead of `Float`. Ignore rounding due to inconsistencies.                                                        | 1061.41 sec    | 126.49% |                                                                                                    |

## Development

> [!NOTE]
> Setup requires Nix and `direnv`

Useful commands during development:

```sh
make build # build and link executable

1brc -f FILE -d # prints final out to stdout as well as debugging info
1brc -f FILE > a.out # creates file with final output

/usr/bin/time -h -p 1brc -f FILE >/dev/null
hyperfine '1brc -f FILE >/dev/null'

lua verify.lua DIR # verify against all test files
```

## Resources

- [Original post](https://www.morling.dev/blog/one-billion-row-challenge)
- [Golang post](https://www.bytesizego.com/blog/one-billion-row-challenge-go)
- [Rust post](https://curiouscoding.nl/posts/1brc)
- [Haskell discourse](https://discourse.haskell.org/t/one-billion-row-challenge-in-hs/8946/217)

