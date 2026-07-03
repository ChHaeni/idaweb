# idaweb

<!-- badges: start -->
<!-- badges: end -->

**idaweb** is an R package that provides a programmatic interface to the [MeteoSwiss Open Data API](https://opendatadocs.meteoswiss.ch/a-data-groundbased) for ground-based meteorological measurements. It lets you search for stations, parameters, and time ranges, and downloads the actual data directly into R.

**NOTE:** This package has been created to help short-cutting the (for my opinion rather cumbersome) way of accessing data through the Open Data Explorer provided by MeteoSwiss. So far it has been only me using it. Feel free to use it, but don't expect things to work the way _you_ are using R (and don't expect any documentation including this README to be well-written (indeed any documentation is now AI-generated, lol)). If you like to use this package and have any improvement suggestions or feature ideas, open an issue or contribute to the package by opening a PR. I prefer _code_ which has been thoroughly thought about (even if it is ugly-looking) over quick-and-dirty-ai coolness by no measures! Any code from me is (and will remain) AI-free.

The package is designed around a simple three-stage workflow:

1. **Search** for the data you need with `met_search()`.
2. **Inspect** the resulting metadata with `stations()`, `parameters()`, and `datainventory()`.
3. **Download** the data with `get_data()`.

All data files are downloaded once and cached locally, so repeated analyses are fast.

---

## Installation

Install the latest development version from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("ChHaeni/idaweb")
```

For compiled versions or a specific release, see the [latest GitHub releases](https://github.com/ChHaeni/idaweb/releases/latest).

---

## Package overview

### Built-in metadata

**idaweb** ships with a pre-packaged metadata catalogue (`metadata`) covering 
all standard ground-based collections:

| Collection | Short Name | Content |
|------------|------------|---------|
| Automatic weather stations | `smn` | Temperature, Precipitation, Wind, Sunshine, Humidity, Radiation and Pressure |
| Automatic precipitation stations | `smn-precip` | Precipitation |
| Automatic tower stations | `smn-tower` | Temperature, Wind, Sunshine, Humidity and Radiation |
| Manual precipitation stations | `nime` | Precipitation and Snow |
| Totaliser precipitation stations | `tot` | Precipitation |
| Pollen stations | `pollen` | Pollen Concentration |
| Meteorological visual observations | `obs` | Visibility, Current and Past Weather, Ground Conditions and Clouds |
| Phenological observations | `phenology` | Phenophases of 26 Plant Species |

You can inspect the metadata directly:

```r
library(idaweb)

metadata
names(metadata)

# Access individual tables
stations(metadata)
parameters(metadata)
datainventory(metadata)
```

### Quick example: Daily air temperature at Zollikofen

```r
library(idaweb)

# 1. Search
mtemp <- met_search(
from = "12.08.2014 to 02.02.2026",
granularity = "D", # daily averages
lon = "7.43..7.49", lat = "46.96..47.12",
group = "Temperature"
)

mtemp
parameters(mtemp)

# 2. Narrow down to 2 m mean temperature
parameters(mtemp[[1]])[, "parameter_description_en"]

mfinal <- met_search(
description = "2mmean",
meta_data = mtemp
)

# 3. Download
zol_temp <- get_data(
meta_data = mfinal,
outclass = "data.table"
)

# The result is a nested list (collection -> station/granularity)
zol_temp[[1]][[1]]
```

### Searching

**By location** — coordinates, altitude, station name, or canton:

```r
# Coordinate box (WGS84)
meta <- met_search(lon = "7.4..7.5", lat = "46.9..47.3")

# Exact station abbreviation
meta <- met_search(abbr = "BER")

# Fuzzy name matching
meta <- met_search(name = "Zurich")

# By canton
meta <- met_search(canton = "BE")
```

Swiss coordinates (LV03 / LV95) are also accepted and automatically converted to WGS84 when the **sf** package is installed.

**By parameter** — group, description, short name, or unit:

```r
# Temperature and precipitation at daily resolution
meta <- met_search(
group = c("temperature", "precipitation"),
granularity = "D"
)

# By exact parameter short name
meta <- met_search(shortname = "tre200s0")

# By description (fuzzy matching)
meta <- met_search(description = "2mmean")
```

**By date and time** — many formats are accepted:

```r
meta <- met_search(from = "01.01.2020", to = "31.12.2020")
meta <- met_search(from = "2020-01-01", to = "2020-12-31")
meta <- met_search(from = "2020")
```

### Downloading data

`get_data()` turns a `met_metadata` object into actual observations.

Output options:
- `outclass`: `data.frame` (default), `data.table`, or `ibts`
- `outstruc`: `split-all` (default), `by-station`, `by-granularity`, or `cbind-all`
- `single_timestamp`: `TRUE` gives a single `time` column; `FALSE` gives `st`/`et`
- `tzone`: Convert timestamps from UTC to another timezone

By default, files are cached in `tempdir()`. For persistence across R sessions, specify a dedicated directory:

```r
dat <- get_data(meta, cache_dir = "C:/meteoswiss_cache")
```

Every returned data object carries metadata attributes:

```r
meta <- met_search(from = '2020', shortname = 'tre200s0', name = 'Zol')
dat <- get_data(meta, outstruc = "cbind-all")
stations(dat)
# -> (station) name has been fuzzy matched |Z||o||l|likofen and |Z|ürich Aff|o||l|tern
parameters(dat)
```

---

## More examples

### 10-minute wind data in the Canton of Jura

```r
meta_jura <- met_search(
from = "12.08.2014", to = "12.08.2014",
canton = "JU",
granularity = "T",
group = "wind"
)

wind_jura <- get_data(meta_jura, outclass = "data.table")
```

### Yearly precipitation in Adelboden

```r
meta_adelb <- met_search(
from = "1999", to = "2025",
abbr = "ADEL",
granularity = "Y",
group = "precipitation"
)

adel_precip <- get_data(
meta_adelb[[1]],
outstruc = "cbind-all",
outclass = "data.table"
)
```

---

## References

- [MeteoSwiss Open Data – Ground−based measurements](https://opendatadocs.meteoswiss.ch/a-data-groundbased)
- [MeteoSwiss Open Data – Data download documentation](https://opendatadocs.meteoswiss.ch/general/download)

---

## Contributing

This package was originally created to streamline personal access to MeteoSwiss data. If you use it and have suggestions, feature ideas, or bug reports, please [open an issue](https://github.com/ChHaeni/idaweb/issues) or submit a pull request.
