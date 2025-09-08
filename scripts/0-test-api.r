
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

##  • collections & meta data ====================

# check supported collections from MeteoSwiss
sup <- supported_collections()
sup

# get meta data
# args(get_metadata)
meta_datainv <- get_metadata(sup, 'data', cache_dir = path_cache)
meta_stations <- get_metadata(sup, 'stat', cache_dir = path_cache)
meta_parameters <- get_metadata(sup, 'par', cache_dir = path_cache)
get_metadata(sup[1])

metadata[[1]][[1]]
metadata[[1]][[2]]
metadata[[1]][[3]]
metadata[[1]][[4]]
metadata[[1]]

##  • search by datetime ====================

zz0 <- search_by_datetime('01.01.2018 to 05.02.2018', meta_data = metadata[7])
zz1 <- search_by_datetime('01.01.2018 to 05.02.2018', meta_data = metadata[10])
zz1b <- search_by_datetime('01.01.2017 to 01.02.2018', meta_data = zz1)
zz2 <- search_by_datetime('01.01.2018 to 05.02.2018', meta_data = metadata)
# search_by_datetime('13.08.2020', '01.01.2018') # -> no longer an error
zz3 <- search_by_datetime('all', '13.08.2020', tz = 'CET', to = '14.08.2020')
# search_by_datetime('07.02.2024/08.03.2025') # -> no longer an error
zz4 <- search_by_datetime(from = '07.02.2024/08.03.2025')
# search_by_datetime(to = '13.08.2020')
# search_by_datetime(from = '01.01.2018', to = '13.08.2020')


##  • search by parameter ====================

pp0 <- search_by_parameter(meta_data = metadata[[7]], granularity = c('T', 'H'),
    description = 'geschw skal m/s', language = 'de')
pz0 <- search_by_datetime(from = '07.02.2024/08.03.2025', meta_data = pp0)

# xx <- search_by_parameter(group = 'wind', granularity = 'T')
# x1 <- search_by_parameter(group = 'wind', granularity = 'T', meta_data = metadata[[7]])
# x2 <- search_by_parameter(group = 'Wind', granularity = 'T', meta_data = metadata[[7]],
#     description = 'geschw skal m/s', language = 'de')
# x3 <- search_by_parameter(meta_data = metadata[[7]], granularity = c('T', 'H'),
#     description = 'geschw skal m/s', language = 'de')

##  • search by location ====================

# Zollikofen
# 2'601'931.15, 1'204'410.72
# 46.990755, 7.464018
ll0 <- search_by_location('7.43..7.49', '46.96..47.12')
ll1 <- search_by_location('7.43..', '..47.12')
ll0b <- search_by_location('7.43..7.49', '46.96..47.12', drop_nodata = TRUE)

# TODO:
#   add option to only return collections with data

# -> search meta_data$stations
# TODO:
#   allow searching by both lv95 & wgs84, even lv03?
#   => convert between coordinate systems -> use sf?
# sf::st_crs('EPSG:4326')
# sf::st_crs('EPSG:2056')
# sf::st_crs('EPSG:21781')
# x <- cbind(c(600000, 620000), c(200000, 220000))
# x1 <- gel::set_crs(x, 'lv03')
# x2 <- gel::set_crs(x, 'lv95')
# sf::sf_project('EPSG:21781', 'EPSG:4326', x)

# TODO: add function to bind different results together

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
# Empty columns are used

if (FALSE) {
    # x1 <- search_by_parameter(group = 'wind', granularity = 'T', meta_data = metadata[[7]])
    # # TODO: pass parameter/station info down the stream
    # xx <- get_filenames(x1)
    # yy <- get_files(xx[1:5])
    # zz_data <- get_data(yy)

    # TODO: station_info(zz_data), parameter_info(zz_data)..

    # x1 <- search_by_parameter(group = c('wind', 'temperature'), granularity = 'H', meta_data = metadata[[7]])
    # x1$parameter

    x1 <- search_by_parameter(shortname = c('fkl010h0', 'tre200h0'), granularity = 'H', meta_data = metadata[[7]])
    head(x1$stations[, 1:16])
    nrow(x1$stations)
    names(x1$stations)
    # qs2::qd_save(x1$stations[, 1:16], '~/repos/5_GitHub/agrammon-workbench/alfam2/idaweb-stations.qdata')

    x2 <- search_by_parameter(shortname = c('fkl010h0', 'tre200h0'), granularity = 'H', 
        meta_data = metadata[[7]])

    # Zollikofen
    # 2'601'931.15, 1'204'410.72
    # 46.990755, 7.464018
    xz <- search_by_location(x2, '7.43..7.49', '46.96..47.12')
    xx <- get_filenames(xz)
    yy <- get_files(xx)
    zz_data <- get_data(yy)
    # qs2::qd_save(zz_data[[1]], '~/repos/5_GitHub/agrammon-workbench/alfam2/zol-temp-ws.qdata')

} when a parameter is not measured at all at a certain station.
