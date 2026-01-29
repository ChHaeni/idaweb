
## header ----------------------------------------

library(idaweb)

# set cache directory
path_cache <- 'tests/cached'
if (!dir.exists(path_cache)) {
    stop('path "', path_cache, '" is not accessible!')
}


## collections & meta data ----------------------------------------

# # fetch info for  available collections
# col <- collections()
# col

# get meta data
# args(get_metadata)

# # works as expected
# meta_datainv <- idaweb:::get_metadata(col, 'data', cache_dir = path_cache)
# meta_stations <- idaweb:::get_metadata(col, 'stat', cache_dir = path_cache)
# meta_parameters <- idaweb:::get_metadata(col, 'par', cache_dir = path_cache)
# xx <- idaweb:::get_metadata(col[1])

# # works as expected
# meta_datainv <- idaweb:::get_metadata(col[1], 'data', cache_dir = path_cache)
# meta_stations <- idaweb:::get_metadata(col[1], 'stat', cache_dir = path_cache)
# meta_parameters <- idaweb:::get_metadata(col[1], 'par', cache_dir = path_cache)
# md <- idaweb:::get_metadata(col[1], cache_dir = path_cache)
# meta_parameters <- idaweb:::get_metadata(col[[1]], 'par', cache_dir = path_cache)


## search by ----------------------------------------

# # works as expected
# dl <- search_by_location('7.43..7.49', '46.96..47.12', meta_data = metadata[1])
# dt <- search_by_datetime('01.09.2024 to 31.12.2025', meta_data = dl)
# p <- parameters(dt[[1]])
# p2 <- parameters(dt[[1]], TRUE)
# s <- stations(dt[[1]])
# s2 <- stations(dt[[1]], TRUE)
# stations(dt[[1]], TRUE)[, station_name]
# datainventory(dt[[1]])
# datainventory(dt[[1]], TRUE)
# parameters(dt[[1]], TRUE)[parameter_granularity == 'H']
# parameters(dt[[1]], TRUE)[parameter_granularity == 'H' & parameter_group_en == 'Radiation']
# parameters(dt[[1]], TRUE)[parameter_granularity == 'H' & parameter_group_en == 'Temperature']
# dp <- search_by_parameter(meta_data = dt, granularity = 'H', 
#     shortname = c('gre000h0', 'tre200h0'))


##  • fix granularity == d,m,y ====================

# # works as expected
# dl <- search_by_location('7.43..7.49', '46.96..47.12', meta_data = metadata[1])
# dt <- search_by_datetime('01.09.2015 to 31.12.2025', meta_data = dl)
# # parameters(dt[[1]], TRUE)[parameter_granularity == 'D' & parameter_group_en == 'Temperature']
# # # fixed ignore case
# # dd <- search_by_parameter(meta_data = dt, granularity = 'd', group = 'temp')
# dd <- search_by_parameter(meta_data = dt, granularity = 'D', group = 'temp')
# # get data
# xx <- get_data(dd, outclass = 'ibts', cache_dir = path_cache)

# # check manual precip data -> ok
# dl <- search_by_location('7.43..7.49', '46.96..47.12', meta_data = metadata[4])
# dt <- search_by_datetime('01.09.2015 to 31.12.2025', meta_data = dl)
# dd <- search_by_parameter(meta_data = dt, granularity = 'D')
# xx <- get_data(dd, outclass = 'ibts', cache_dir = path_cache)

# dl <- search_by_location('7.43..7.49', '46.96..47.12', meta_data = metadata[1])
# dt <- search_by_datetime('01.09.2015 to 31.12.2025', meta_data = dl)
# dd <- search_by_parameter(meta_data = dt, granularity = 'M')
# xx <- get_data(dd, outclass = 'ibts', cache_dir = path_cache)

# dl <- search_by_location('7.43..7.49', '46.96..47.12', meta_data = metadata[1])
# dt <- search_by_datetime('01.09.2015 to 31.12.2025', meta_data = dl)
# dd <- search_by_parameter(meta_data = dt, granularity = 'Y')
# xx <- get_data(dd, outclass = 'ibts', cache_dir = path_cache)

# # test hourly data from 2015 to 2025 -> ok
# dl <- search_by_location('7.4..7.49', '46.9..47.2', meta_data = metadata[1])
# dt <- search_by_datetime('01.09.2015 to 31.12.2025', meta_data = dl)
# dd <- search_by_parameter(meta_data = dt, granularity = c('h', 'm'))
# # xx <- get_data(dd, outclass = 'ibts', cache_dir = path_cache)
# # xx <- get_data(dd, outclass = 'ib', outstruc = 'cbind', cache_dir = path_cache)
# # xx <- get_data(dd, outclass = 'dt', outstruc = 'cbind', cache_dir = path_cache)
# # xx <- get_data(dd, outclass = 'dt', outstruc = 'by-granularity', cache_dir = path_cache)
# # xx <- get_data(dd, outclass = 'dt', outstruc = 'by-station', cache_dir = path_cache)
# # xx <- get_data(dd, outclass = 'df', outstruc = 'cbind', cache_dir = path_cache)
# # parameters(xx)
# # stations(xx)
# # yy <- get_data(dd, outclass = 'df', outstruc = 'by-station', cache_dir = path_cache)
# zz <- get_data(dd, outclass = 'df', outstruc = 'by-gr', cache_dir = path_cache)
# parameters(zz)
# stations(zz)

# TODO (maybe): add function to switch between different structures

# using CH-coords
# dl <- search_by_location('620..650', '190..210', meta_data = metadata[1])
dl <- search_by_location('600..602', '203..205', meta_data = metadata[1])

## old tests ----------------------------------------


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
