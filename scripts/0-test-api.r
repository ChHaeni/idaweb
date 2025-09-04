
## header ----------------------------------------

# set wd
setwd('~/repos/3_Scripts/8_meteoswiss')

# source functions
source('scripts/idaweb-functions.r')

# load meta data
load('data/metadata.rda')

# set cache directory
path_cache <- 'cached'
if (!dir.exists(path_cache)) {
    dir.create(path_cache, recursive = TRUE)
}

## testing ----------------------------------------

# check supported collections from MeteoSwiss
sup <- supported_collections()
sup

# get meta data
# args(get_metadata)
meta_datainv <- get_metadata(sup, 'data', cache_dir = path_cache)
meta_stations <- get_metadata(sup, 'stat', cache_dir = path_cache)
meta_parameters <- get_metadata(sup, 'par', cache_dir = path_cache)
get_metadata(sup[1])

## hier weiter!!!
# TODO:
#   search functions
#   search by: time range, location range, parameters (fuzzy search)


# # zz1 <- search_by_datetime('01.01.2018 to 05.02.2018', meta_search = metadata[10])
# zz1 <- search_by_datetime('01.01.2018 to 05.02.2018', meta_search = metadata[7])
# zz1b <- search_by_datetime('01.01.2017 to 01.02.2018', meta_search = zz1)
# zz2 <- search_by_datetime('01.01.2018 to 05.02.2018', meta_search = metadata[[7]])
# search_by_datetime('01.01.2018 to 05.02.2018', meta_search = metadata)
# search_by_datetime('smn', '13.08.2020')
# search_by_datetime('13.08.2020', '01.01.2018') # -> error
# zz3 <- search_by_datetime('all', '13.08.2020', tz = 'CET', to = '14.08.2020')
# search_by_datetime('07.02.2024/08.03.2025')
# search_by_datetime(to = '13.08.2020')
# search_by_datetime(from = '01.01.2018', to = '13.08.2020')

# zz1 <- search_by_datetime(metadata[[7]], '01.01.2018 to 05.02.2018')
# x3 <- search_by_parameter(meta_search = metadata[[7]], granularity = c('T', 'H'),
#     description = 'geschw skal m/s', language = 'de')

# xx <- search_by_parameter(group = 'wind', granularity = 'T')
# x1 <- search_by_parameter(group = 'wind', granularity = 'T', meta_search = metadata[[7]])
# x2 <- search_by_parameter(group = 'Wind', granularity = 'T', meta_search = metadata[[7]],
#     description = 'geschw skal m/s', language = 'de')
# x3 <- search_by_parameter(meta_search = metadata[[7]], granularity = c('T', 'H'),
#     description = 'geschw skal m/s', language = 'de')

# add option to provide previous results for further subsetting
# add function to bind different results together
# add function to get data from results

## testing ----------------------------------------

# # base url to REST API / GET search
# valid_collections <- paste(
#     # 'ch.meteoschweiz.ogd-nbcn',
#     # 'ch.meteoschweiz.ogd-nbcn-precip',
#     # 'ch.meteoschweiz.ogd-nime',
#     # 'ch.meteoschweiz.ogd-obs',
#     # 'ch.meteoschweiz.ogd-phenology',
#     # 'ch.meteoschweiz.ogd-pollen',
#     'ch.meteoschweiz.ogd-smn',
#     'ch.meteoschweiz.ogd-smn-precip',
#     'ch.meteoschweiz.ogd-smn-tower',
#     'ch.meteoschweiz.ogd-tot',
#     sep = ','
# )
# bu <- paste0('https://data.geo.admin.ch/api/stac/v1/search?collections=', 
#     valid_collections, '&')
# # t1 <- content(GET(paste0(bu, 'bbox=6.96,45.82,9,46.81')))
# bern <- c(46.989090, 7.463082)
# ll <- c(46.979143, 7.445185)
# ur <- c(46.997198, 7.482103)
# bbox <- paste(c(rev(ll), rev(ur)), collapse = ',')
# t1 <- content(GET(paste0(bu, 'bbox=', bbox, '&datetime=2024-01-01T00:00:00Z/..')))
# # names(t1)
# length(t1$features)
# # NOTE: always limit of 100!!
# names(t1$features[[1]]$assets)
# str(t1)

# https://opendatadocs.meteoswiss.ch/general/download#update-frequency
# historical    (meas. start until 31.12 last year): once a year        (m, d, h, t)
# recent        (1.1. current year until yesterday): daily at 12UTC     (m, d, h, t)
# now           (yesterday 12UTC to now):            every 10 min       (h, t)
# no type

# m: monthly, d: daily, h: hourly, t: 10-min

# for granularity t and h the time stamp defines the end of the measurement interval and
# for higher granularities (d, m and y) the time stamp defines the beginning of the interval!

# Missing values (e.g. due to instrument failure) are empty fields. 
# Empty columns are used when a parameter is not measured at all at a certain station.
