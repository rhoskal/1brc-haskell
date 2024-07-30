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

| Attempt | Approach                                                                                                                                                                                                                               | Execution Time |    Diff | Commit                                                                                             |
| :-----: | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------- | ------: | -------------------------------------------------------------------------------------------------- |
|    0    | Naïve Implementation: Read temperatures into a Map of cities. Iterate serially over each key (station name) in Map to and corresponding list of temperaturs to calculate min, max and mean temperatures. Uses non-performant `String`. | 4714.17 sec    |         | [a58c42d](https://github.com/rhoskal/1brc-haskell/commit/a58c42dcb0b2f414fdfbb1a503777dc42ade1fd2) |
|    1    | Parser uses `Text` instead of `String`, read file as `ByteString` instead of `String`. Store temperatures as `IntX` instead of `Float`. Ignore rounding due to inconsistencies.                                                        | 1061.41 sec    | 126.49% | [1550352](https://github.com/rhoskal/1brc-haskell/commit/155035264f747254267488c4ea4ea13a7a670538) |
|    2    | Add strictness and compiler flags for small performance improvements. Read file content lazily.                                                                                                                                        | 969.98 sec     |   9.00% | [00ddd57](https://github.com/rhoskal/1brc-haskell/commit/00ddd571360f5cd60e90b9a55ab8bb7ed8914f25) |
|    3    | Replace `State` monad with `List.foldl'`.                                                                                                                                                                                              | 970.47 sec     |  -0.05% | [75211bb](https://github.com/rhoskal/1brc-haskell/commit/75211bbd93afc3ec32f1661aa2e3b5b500b184bf) |
|    4    | Ditch fancy, custom monad `Parser` for a down 'n dirty parser all in the name of speed. 😢                                                                                                                                             | 875.62 sec     |  10.28% | [e2df7f6](https://github.com/rhoskal/1brc-haskell/commit/e2df7f6b23a8518689ede3d458a734cc6f0db080) |
|    5    | Parser, formatter and printer all use `ByteString`.                                                                                                                                                                                    | 372.68 sec     |  80.58% | [35ac275](https://github.com/rhoskal/1brc-haskell/commit/35ac275d3895ec9700f5ee6040a2c5b47ea93dc8) |
|    6    | Parse file in chunks.                                                                                                                                                                                                                  | 154.57 sec     |  82.73% | [35860b1](https://github.com/rhoskal/1brc-haskell/commit/35860b10b4603e1f9e688c983fc4fd768b0a1fd6) |
|    7    | Proper floating point rounding.                                                                                                                                                                                                        | 155.14 sec     |   0.36% | [22739ec](https://github.com/rhoskal/1brc-haskell/commit/22739ec44aa92c06d357fdca67a24b32cff9ff55) |

## Show & Tell

```sh
λ 1brc --help
1 Billion Row Challenge

Usage: 1brc [--version] [-d|--debug] (-f|--file FILE_PATH)
            [-c|--csize CHUNK_SIZE]

  Run the current aggregation implementation optimized for speed.

Available options:
  -h,--help                Show this help text
  --version                Show version
  -d,--debug               Output information useful for debugging
  -f,--file FILE_PATH      Path to measurements file
  -c,--csize CHUNK_SIZE    Chunk size in bytes (default: 64000000)

For more information, please visit https://1brc.dev
```

```sh
λ 1brc -f ../data/measurements-10.txt -d
2024-07-30 19:58:03.149119: [debug] Running v7...
@(lib/Run.hs:79:3)
2024-07-30 19:58:03.156886: [debug] First 10 processed:
* (Station {unStation = "Alexandra"},Summary {sMin = 0.6, sMax = 0.6, sTotal = 0.6, sCount = 1})
* (Station {unStation = "Benghazi"},Summary {sMin = 7.5, sMax = 7.5, sTotal = 7.5, sCount = 1})
* (Station {unStation = "Blantyre"},Summary {sMin = 13.1, sMax = 13.1, sTotal = 13.1, sCount = 1})
* (Station {unStation = "Bouak\195\169"},Summary {sMin = 16.3, sMax = 16.3, sTotal = 16.3, sCount = 1})
* (Station {unStation = "Lyon"},Summary {sMin = 5.3, sMax = 5.3, sTotal = 5.3, sCount = 1})
* (Station {unStation = "Napoli"},Summary {sMin = 21.3, sMax = 21.3, sTotal = 21.3, sCount = 1})
* (Station {unStation = "Niamey"},Summary {sMin = 19.2, sMax = 19.2, sTotal = 19.2, sCount = 1})
* (Station {unStation = "Port Vila"},Summary {sMin = 32.2, sMax = 32.2, sTotal = 32.2, sCount = 1})
* (Station {unStation = "S\195\169gou"},Summary {sMin = 19.8, sMax = 19.8, sTotal = 19.8, sCount = 1})
* (Station {unStation = "Vladivostok"},Summary {sMin = 15.5, sMax = 15.5, sTotal = 15.5, sCount = 1})
@(lib/Run.hs:83:3)
{Alexandra=0.6/0.6/0.6, Benghazi=7.5/7.5/7.5, Blantyre=13.1/13.1/13.1, Bouaké=16.3/16.3/16.3, Lyon=5.3/5.3/5.3, Napoli=21.3/21.3/21.3, Niamey=19.2/19.2/19.2, Port Vila=32.2/32.2/32.2, Ségou=19.8/19.8/19.8, Vladivostok=15.5/15.5/15.5}
```

```sh
λ /usr/bin/time -h -p 1brc -f ../data/measurements-1000000000.txt >/dev/null
real 156.82
user 153.50
sys 1.10
```

```sh
λ lua verify.lua ../data
Verifying checksums: ../data/measurements-boundaries.txt
Verifying checksums: ../data/measurements-complex-utf8.txt
Verifying checksums: ../data/measurements-dot.txt
Verifying checksums: ../data/measurements-short.txt
Verifying checksums: ../data/measurements-shortest.txt
Verifying checksums: ../data/measurements-1.txt
Verifying checksums: ../data/measurements-10.txt
Verifying checksums: ../data/measurements-100.txt
💥
Expected: 3f8ac812e47ee6b153ab5e2fee1e2fcefd3521b2
Received: f625ca2d72ed36a79e1302b4195c5f7774d86af6
```

## Profiling

```sh
1brc +RTS -s -RTS -f FILE >/dev/null
```

[Alexis King](https://www.youtube.com/watch?v=yRVjR9XcuPU&ab_channel=Tweag) suggests using the following flags to help understand GHC core:

```
-ddump-to-file
-ddump-simpl
-dsuppress-coercions
-dsuppress-module-prefixes
-dsuppress-type-applications
```

## Benchmarking

```sh
cabal run exe:bench
cabal run exe:bench -- --match "prefix" "Group name"
```

## Resources

- [Original post](https://www.morling.dev/blog/one-billion-row-challenge)
- [Golang post](https://www.bytesizego.com/blog/one-billion-row-challenge-go)
- [Rust post](https://curiouscoding.nl/posts/1brc)
- [Haskell discourse](https://discourse.haskell.org/t/one-billion-row-challenge-in-hs/8946/217)
