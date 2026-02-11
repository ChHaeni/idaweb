# idaweb - an R package to facilitate searching and downloading MeteoSwiss Ground-based measurements
The package idaweb helps accessing MeteoSwiss Open Data (Ground-based measurements) through R.
NOTE: This package has been created to help short-cutting the (for me rather cumbersome) way of accessing data through the Open Data Explorer provided by MeteoSwiss. So far it has been only me using it. Feel free to use it, but don't expect things to work the way you are using R (and don't expect any documentation to be well-written and timely introduced). If you like to use this package and have any improvement suggestions or feature ideas, open an issue or contribute to the package by opening a PR.

## Installation

```r
remotes::install_github('ChHaeni/idaweb')
```

## Usage examples

### Available Data Collections

```r
# attach idaweb package
library(idaweb)

# collections meta data is available with the package as list object
print(metadata)

# list entry names match collection names
names(metadata)

# # it is possible to fetch available collections, although not really needed
# met_cols <- collections()
# print(met_cols)
```

### Accessing Meta Data

```r
# access station meta data
stations(metadata)
# parameter meta data
# data inventory meta data
```

### Searching Stations By Location

```r
args(search_by_location)
```

