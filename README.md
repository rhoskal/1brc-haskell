# The Billion Row Challenge (1BRC)

1️⃣🐝🏎️ [The One Billion Row Challenge](https://github.com/gunnarmorling/1brc) -- A fun exploration of how quickly 1B rows from a text file can be aggregated.

## The problem

**Input**: a 13GB file containing lines of the form `Amsterdam;10.5`.

- First, a city name. There are cities, and each row has name chosen at random from the set. Their lengths are from **3** to **26** bytes.
- Then a temperature, formatted as `-?\d?\d,\d`, i.e. a possibly negative number with one or two integral digits and exactly one decimal. Each temperature is drawn from a normal distribution for each city.

**Output**: a sorted list of cities of the form `<city>: <min>/<avg>/<max>`, each formatted with one decimal place.

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

| Attempt Number | Approach | Execution Time | Diff | Commit |
|----------------|----------|----------------|------|--------|
|0| Naive Implementation: Read temperatures into a map of cities. Iterate serially over each key (city) in map to find min, max and average temperatures.| | ||

