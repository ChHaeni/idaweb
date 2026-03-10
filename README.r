# # idaweb - an R package to facilitate searching and downloading MeteoSwiss Ground-based measurements
# The package idaweb helps accessing MeteoSwiss Open Data (Ground-based measurements) through R.
# NOTE: This package has been created to help short-cutting the (for me rather cumbersome) way of accessing data through the Open Data Explorer provided by MeteoSwiss. So far it has been only me using it. Feel free to use it, but don't expect things to work the way you are using R (and don't expect any documentation to be well-written and timely introduced). If you like to use this package and have any improvement suggestions or feature ideas, open an issue or contribute to the package by opening a PR.

# ## Installation

remotes::install_github('ChHaeni/idaweb')

# attach idaweb package for examples below
library(idaweb)

# ## Usage examples

# ### 1. Daily Averages Of Air Temperature At Zollikofen Meteostation between 12.08.2014 and 02.02.2026

# search for daily averages, within the bounding box of 7.43\u00B0E/46.96\u00B0N
# and 7.49\u00B0E/47.12\u00B0N, between 12.08.2014 and 02.02.2026,
# and within the group matching "Temperature"
mtemp <- met_search(
      granularity = 'D' # daily averages
    , lon = '7.43..7.49' # select range around Zollikofen station (for demonstration purpose)
    , lat = '46.96..47.12' # dito
    , from = '12.08.2014 to 02.02.2026' # this could also be single entries from/to (see example 3.)
    , group = 'Temperature'
)

# check content by printing to terminal
mtemp
# -> a list with 1 list entry (i.e. 1 collection)
# -> 1 Meteostation (Zollikofen)
# -> 9 Parameters => check

# check parameters
parameters(mtemp)

# check hidden parameter description
parameters(mtemp[[1]])[, 'parameter_description_en']

# subset further -> we want air temperature at 2 m a.g.l., daily mean
mfinal <- met_search(
    description = '2mmean', # description uses fuzzy matching
    language = 'en', # english is default and doesn't need to be specified
    meta_data = mtemp # provided previously filtered metadata
)

# check parameters -> ok
parameters(mfinal)

# get (download) data
zol_temp <- get_data(
      meta_data = mfinal # final filtered metadata
    # , cache_dir = 'some/path' # cache directory can be specified to reuse data even after quitting R session
    , outclass = 'data.table' # we like data.table! ;-)
)

# check data
# NOTE: it is still a list (collection) of lists (default: station/granularity). This might be a bit confusing at first - but it all makes sense when downloading from several collections, stations, granularities etc...
zol_temp[[1]][[1]]

# FIXME: provide single entry meta_data (mtemp[[1]])
# TODO: fix to -> to + 1 day, if only date is provided(?)

# ### 2. Ten-Minute Averages Of Wind Speed and Wind Direction At Meteostations in the Kanton of Jura for 12.08.2014

# ### 3. Yearly Precipitation in Adelboden between 1999 and 2025

# ## Further "Explanation" Of Individual Functions

# ### Available Data Collections


# collections meta data is available with the package as list object
print(metadata)

# list entry names match collection names
names(metadata)

# # it is possible to fetch available collections, although not really needed
# met_cols <- collections()
# print(met_cols)

# ### Accessing Meta Data

# access station meta data
stations(metadata)
# parameter meta data
parameters(metadata)
# data inventory meta data
datainventory(metadata)

# ### Searching Stations By Location

args(search_by_location)

