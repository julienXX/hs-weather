weather
---

Get the current weather in a city from the command line.
Perfect for some prompt swag!

![prompt.png](https://s3.amazonaws.com/f.cl.ly/items/0j0A0o0c0c0F0g3B2I2b/Capture%20d%E2%80%99e%CC%81cran%202015-05-17%20a%CC%80%2016.24.11.png)

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
λ git clone https://github.com/julienXX/weather.git
λ cd weather
λ cabal sandbox init
λ cabal install
λ cabal build
```

Then copy `dist/build/weather/weather` wherever it fancies you.
