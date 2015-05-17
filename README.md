weather
---

Get the current weather in a city from the command line.

Usage
===

```
λ weather "Paris, France"
☀️
```

```
λ weather "London"
💦
```

Installation
===

```shell
λ git clone https://github.com/julienXX/hs-weather.git
λ cd hs-weather
λ cabal sandbox init
λ cabal install
λ cabal build
```

Then copy `dist/build/weather/weather` wherever it fancies you.
