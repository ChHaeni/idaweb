
# # # save metadata
# xx <- supported_collections()
# # z1 <- get_metadata(xx, 'data')
# # z2 <- get_metadata(xx, 'stat')
# # z3 <- get_metadata(xx, 'par')
# z1 <- get_metadata(xx, 'data', cache_dir = 'cached')
# z2 <- get_metadata(xx, 'stat', cache_dir = 'cached')
# z3 <- get_metadata(xx, 'par', cache_dir = 'cached')
# metadata <- mapply(\(col, inv, stat, para) {
#     list(
#         assets = col$assets,
#         datainventory = inv,
#         stations = stat,
#         parameters = para
#     )}, attr(xx, 'collections'), z1, z2, z3, SIMPLIFY = FALSE)
# names(metadata) <- xx
# save(metadata, file = 'data/metadata.rda')
# sapply(metadata, \(x) sapply(x$assets, '[[', 'file:checksum'))
# get_metadata(xx[1])

## hier weiter!!!
# TODO:
#   search functions
#   search by: time range, location range, parameters (fuzzy search)

load('data/metadata.rda')

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

