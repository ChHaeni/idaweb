


## NOTES ----------------------------------------

# REST API docu
# https://data.geo.admin.ch/api/stac/static/spec/v1/api.html#tag/Data

# only collections from meteoswiss
# only ids containing ogd (because of meta data & data format)

# in general:
# - fetch data info
# - meta data included in package -> check if update needed
# - function to update specific or all meta data (if necessary)
# - download data only if not available in options
# - one function to get data (incl. options check)

## functions ----------------------------------------

##  • header ====================

library(httr)

# add helper function to construct url
ms_url <- function(...) {
    paste0('https://data.geo.admin.ch/', ...)
}

##  • main functions ====================

# fetch available MeteoSwiss Open Data
# run to show all supported collections: collections(TRUE)
collections <- function(supported_only = FALSE) {
    # base url to REST API
    get_url <- ms_url('api/stac/v1/collections?provider=meteoswiss')
    # get info
    out <- content(GET(get_url))
    if (is.null(out$code)) {
        ids <- sapply(out$collections, '[[', 'id')
        if (supported_only) {
            # filter for collections supported by this package
            i_supported <- grep('ogd-forecasting(*SKIP)(*FAIL)|ogd-.*', ids, perl = TRUE)
            out$collections <- out$collections[i_supported]
            ids <- ids[i_supported]
        }
        attr(ids, 'collections') <- out$collections
        structure(ids, class = 'ms_collections')
    } else {
        out
    }
}

# function to get info on specific data set(s) from collection
# e.g.: info(collections())
# x -> either ms_collections object or a valid id
info <- function(x, i = NULL) {
}

# get supported collections
supported_collections <- function() {
    out <- getOption('ms_supported_collections')
    if (is.null(out)) {
        # get collections
        out <- collections(TRUE)
        # assign to options
        options(setNames(list(out), 'ms_supported_collections'))
    }
    invisible(out)
}

# get meta data
get_metadata <- function(id, type = c('datainventory', 'stations', 'parameters'),
    cache_dir = tempdir()) {
    type <- match.arg(type)
    if (length(id) == 1L) {
        # check supported id
        if (i_supp <- check_supported(id)) {
            # get collection info
            id_coll <- attr(i_supp, 'collection')
            # get file update
            file_name <- grep(type, names(id_coll$assets), value = TRUE, 
                fixed = TRUE)
            last_updated <- lubridate::fast_strptime(
                id_coll$assets[[file_name]]$updated,
                format = "%Y-%m-%dT%H:%M:%OSZ", lt = FALSE
            )
            # check if package data is up-to-date
            # if (last_updated > idaweb:::metadata[[id]]$assets[[file_name]]$updated) {
            # replace me when package
            load('data/metadata.rda')
            if (last_updated > metadata[[id]]$assets[[file_name]]$updated) {
                # update data...
                # get url & checksum
                file_url <- id_coll$assets[[file_name]]$href
                file_checksum <- id_coll$assets[[file_name]][['file:checksum']]
                if (!(basename(file_url) %in% names(.Options))) {
                    # TODO: add message about github issue (only first time if local
                    #           file does not exist yet)
                    # cat()
                    warning('TODO here!')
                    # ?
                }
                # download file
                local_file <- dl_data(file_url, file_checksum, cache_dir = cache_dir)
                out <- read.table(local_file, sep = ';', header = TRUE, fill = TRUE,
                        fileEncoding = 'Windows-1252', comment.char = '', quote = '"'
                    )
                # convert datetimes
                switch(type,
                    datainventory = {
                        out$data_since <- lubridate::fast_strptime(out$data_since, 
                            format = '%d.%m.%Y %H:%M', tz = 'UTC', lt = FALSE)
                        if (is.logical(out$data_till)) {
                            out$data_till <- as.POSIXct(out$data_till)
                        } else {
                            out$data_till <- lubridate::fast_strptime(out$data_till, 
                                format = '%d.%m.%Y %H:%M', tz = 'UTC', lt = FALSE)
                        }
                    },
                    stations = {
                        out$station_data_since <- lubridate::fast_strptime(
                            out$station_data_since, format = '%d.%m.%Y', 
                            tz = 'UTC', lt = FALSE)
                    }
                )
                return(out)
            }
            # return(idaweb:::metadata[[id]][[file_name]])
            # replace me when package
            return(metadata[[id]][[file_name]])
        } else {
            return(invisible(NULL))
        }
    } else {
        # get data
        out <- lapply(id, get_metadata, type = type, cache_dir = cache_dir)
        # remove invalid
        return(out[!sapply(out, is.null)])
    }
}

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

search_by_datetime <- function(meta_search, from, to, tz = get_tzone(from, to)) {
    # change once package
    if (missing(meta_search)) {
        # meta_search <- idaweb:::metadata
        meta_search <- metadata
    } else if (is.character(meta_search)) {
        # ind <- sub('ch.meteoschweiz.ogd-', '', names(idaweb:::metadata)) %in%
        ind <- sub('ch.meteoschweiz.ogd-', '', names(metadata)) %in%
            sub('ch.meteoschweiz.ogd-', '', meta_search)
        if (any(ind)) {
            # collection name(s)
            if (length(meta_search) == 1L) {
                meta_search <- metadata[[which(ind)]]
            } else {
                meta_search <- metadata[ind]
            }
        } else {
            meta_search <- switch(meta_search
                # 'all' = idaweb:::metadata,
                , 'all' = metadata
                # any others?
                # error unknown
                , stop('cannot interpret argument "meta_search"!')
            )
        }
    }
    # change argument meta_search to meta_search = idaweb:::metadata or similar argument name
    # parse from & to
    fromto <- check_fromto(from, to, tz = tz)
    # select datainventory/station/parameters
    if ('datainventory' %in% names(meta_search)) {
        if (!is.null(sft <- attr(meta_search, 'search_fromto'))) {
            # check from
            fromto[1] <- max(sft[1], fromto[1])
            # check to
            fromto[2] <- min(sft[2], fromto[2])
        }
        # TODO: improve these if/else tests! and capture errors
        # check from
        i_from <- is.na(meta_search$datainventory$data_till) | 
            fromto[1] <= meta_search$datainventory$data_till
        # check to
        i_to <- i_from & meta_search$datainventory$data_since <= fromto[2]
        # return subset incl from/to
        sub_inv <- meta_search$datainventory[i_to, ]
        # get stations
        sub_stats <- meta_search$stations[meta_search$stations$station_abbr %in% 
            sub_inv$station_abbr, ]
        # get parameters
        sub_paras <- meta_search$parameters[meta_search$parameters$parameter_shortname %in% 
            sub_inv$parameter_shortname, ]
        structure(
            list(
                assets = meta_search$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'meta_search', 
            # get since & till
            data_since = min(sub_inv$data_since),
            data_till = max(sub_inv$data_till),
            wgs84_lat = range(sub_stats$station_coordinates_wgs84_lat),
            wgs84_lon = range(sub_stats$station_coordinates_wgs84_lon),
            parameters = unique(sub_paras$parameter_shortname),
            collection = basename(dirname(meta_search$assets[[1]]$href)),
            search_fromto = list(fromto = fromto, tz = tz),
            search_location = attr(meta_search, 'search_location'),
            search_parameters = attr(meta_search, 'search_parameters')
        )
    } else {
        sapply(meta_search, search_by_datetime, from = fromto[1], to = fromto[2], tz = tz,
            simplify = FALSE)
    }
}

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

search_by_location <- function(meta_search, x, y) {
    # valid search entries:
    # lat & lon: '46.1..46.2', '46.1 to 46.2', '46.1/46.2', c(46.1, 46.2), 
    # ch_x & ch_y: same as above BUT additionally, only 100-thousands 
    #   -> distinguish between lv95 and lv03
    #   -> check x/y as required in R and possibly flip
    # only attach sf if really necessary
    # change once package
    if (missing(meta_search)) {
        # meta_search <- idaweb:::metadata
        meta_search <- metadata
    } else if (is.character(meta_search)) {
        # ind <- sub('ch.meteoschweiz.ogd-', '', names(idaweb:::metadata)) %in%
        ind <- sub('ch.meteoschweiz.ogd-', '', names(metadata)) %in%
            sub('ch.meteoschweiz.ogd-', '', meta_search)
        if (any(ind)) {
            # collection name(s)
            if (length(meta_search) == 1L) {
                meta_search <- metadata[[which(ind)]]
            } else {
                meta_search <- metadata[ind]
            }
        } else {
            meta_search <- switch(meta_search
                # 'all' = idaweb:::metadata,
                , 'all' = metadata
                # any others?
                # error unknown
                , stop('cannot interpret argument "meta_search"!')
            )
        }
    }
    # change argument meta_search to meta_search = idaweb:::metadata or similar argument name
    seps <- c('[.][.]', 'to', '/', '//')
    # parse x
    # check list format
    if (is.list(x)) {
        stop('Fix list input!')
        ul <- unique(lengths(x))
        if (length(ul) > 1 || ul < 1 || ul > 2) {
            stop('x list input not valid')
        }
    }
    # check separators
    xl <- strsplit(x, split = paste(seps, collapse = '|'))
    # parse y
    # check list format
    if (is.list(y)) {
        stop('Fix list input!')
        ul <- unique(lengths(y))
        if (length(ul) > 1 || ul < 1 || ul > 2) {
            stop('y list input not valid')
        }
    }
    # check separators
    yl <- strsplit(y, split = paste(seps, collapse = '|'))
    # fix coord values
    xv <- lapply(xl, \(z) {
        v <- as.numeric(z)
        if (all(v < 20)) {
            # lon
            # v is ok
        } else {
            stop('Fix ch coordinates')
            # 200/200000, 1200/1200000
        }
        v
    })
    yv <- lapply(yl, \(z) {
        v <- as.numeric(z)
        if (all(v > 20 & v < 50)) {
            # lon
            # v is ok
        } else {
            stop('Fix ch coordinates')
            # 200/200000, 1200/1200000
        }
        v
    })
    # select datainventory/station/parameters
    if ('datainventory' %in% names(meta_search)) {
        if (!is.null(sft <- attr(meta_search, 'search_location'))) {
            stop('Fix already searched by location')
        }
        # TODO: improve these if/else tests! and capture errors
        # check from
        s_lon <- meta_search$stations$station_coordinates_wgs84_lon 
        s_lat <- meta_search$stations$station_coordinates_wgs84_lat 
        i_x <- unlist(lapply(xv, \(v) s_lon >= v[1] & s_lon <= v[2]))
        i_y <- unlist(lapply(yv, \(v) s_lat >= v[1] & s_lat <= v[2]))
        i_ok <- i_x & i_y
        # return subset of stations
        sub_stats <- meta_search$stations[i_ok, ]
        # get inventory
        sub_inv <- meta_search$datainventory[meta_search$datainventory$station_abbr 
            %in% sub_stats$station_abbr, ]
        # get parameters
        sub_paras <- meta_search$parameters[meta_search$parameters$parameter_shortname %in% 
            sub_inv$parameter_shortname, ]
        structure(
            list(
                assets = meta_search$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'meta_search', 
            # get since & till
            data_since = min(sub_inv$data_since),
            data_till = max(sub_inv$data_till),
            wgs84_lat = range(sub_stats$station_coordinates_wgs84_lat),
            wgs84_lon = range(sub_stats$station_coordinates_wgs84_lon),
            parameters = unique(sub_paras$parameter_shortname),
            collection = basename(dirname(meta_search$assets[[1]]$href)),
            search_fromto = attr(meta_search, 'search_fromto'),
            search_location = list(x = x, y = y),
            search_parameters = attr(meta_search, 'search_parameters')
        )
    } else {
        sapply(meta_search, search_by_location, x = xv, y = yv, simplify = FALSE)
    }
}
# -> search meta_search$stations
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

search_by_parameter <- function(shortname, unit, group, description, 
    language = c('en', 'de', 'fr', 'it'), 
    granularity = c('T', 'H', 'D', 'M', 'Y'), 
    meta_search = metadata) {
    if ('datainventory' %in% names(meta_search)) {
        sub_paras <- meta_search$parameter
        language <- match.arg(language)
        search_parameters <- list()
        if (!missing(shortname)) {
            # search by shortname (vector)
            ind <- unlist(lapply(shortname, grep, sub_paras$parameter_shortname))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(shortname = shortname))
        }
        if (!missing(unit)) {
            # search by unit (vector)
            ind <- unlist(lapply(unit, grep, sub_paras$parameter_unit))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(unit = unit))
        }
        if (!missing(group)) {
            # search by group (vector)
            ind <- unlist(lapply(group, grep, sub_paras[[paste0('parameter_group_', 
                    language)]]))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(group = group))
        }
        if (!missing(granularity)) {
            # search by granularity (vector)
            ind <- sub_paras$parameter_granularity %in% granularity
            sub_paras <- sub_paras[ind, ]
            search_parameters <- c(search_parameters, list(granularity = granularity))
        }
        if (!missing(description)) {
            # search by group (vector)
            ind <- unlist(lapply(description, fuzzy_search, 
                sub_paras[[paste0('parameter_description_', language)]]))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(description = description))
        }
        # get inventory
        sub_inv <- meta_search$datainventory[meta_search$datainventory$parameter_shortname %in% 
            sub_paras$parameter_shortname, ]
        # get stations
        sub_stats <- meta_search$stations[meta_search$stations$station_abbr %in% 
            sub_inv$station_abbr, ]
        if (nrow(sub_inv) > 0) {
            data_since <- min(sub_inv$data_since)
            data_till <- max(sub_inv$data_till)
            wgs84_lat <- range(sub_stats$station_coordinates_wgs84_lat)
            wgs84_lon <- range(sub_stats$station_coordinates_wgs84_lon)
            parameters <- unique(sub_paras$parameter_shortname)
        } else {
            data_since <- lubridate::NA_POSIXct_
            data_till <- NA_integer_
            wgs84_lat <- c(NA_real_, NA_real_)
            wgs84_lon <- c(NA_real_, NA_real_)
            parameters <- NA_character_
        }
        structure(
            list(
                assets = meta_search$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'meta_search', 
            # get since & till
            data_since = data_since,
            data_till = data_till,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            collection = basename(dirname(meta_search$assets[[1]]$href)),
            search_fromto = attr(meta_search, 'search_fromto'),
            search_location = attr(meta_search, 'search_location'),
            search_parameters = search_parameters
        )
    } else {
        sapply(meta_search, search_by_parameter, shortname = shortname, unit = unit, 
            group = group, description = description, language = language, 
            granularity = granularity, simplify = FALSE)
    }
}

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

# -> convenience functions => show_stations, show_parameters

.get_filenames <- function(x, from, to, now, pre, yd12, cy_jan) {
    # update frequency (https://opendatadocs.meteoswiss.ch/general/download#update-frequency)
    # historical    (meas. start until 31.12 last year): once a year        (m, d, h, t)
    # recent        (1.1. current year until yesterday): daily at 12UTC     (m, d, h, t)
    # now           (yesterday 12UTC to now):            every 10 min       (h, t)
    # no type                                            varies             (e.g. y)
    file_list <- list()
    # station
    stat <- tolower(x[['station_abbr']][1])
    # granularity
    gran <- tolower(x[['parameter_granularity']][1])
    if (gran == 'y') {
        # -> check file names! => do they always look the same?
        return(paste(pre, stat, 'y.csv', sep = '_'))
    } else if (gran %in% c('t', 'h')) {
        if (is.na(to)) {
            to <- now
        }
        # add yesterday cut
        if (from <= yd12 && to > yd12) {
            file_list <- c(file_list, list(list(
                    filename = paste(pre, stat, gran, 'now.csv', sep = '_'),
                    from = max(from, yd12),
                    to = min(to, now)
                )))
        }
    }
    # add previous year cut
    if (from <= cy_jan && to > cy_jan) {
        file_list <- c(file_list, list(list(
                filename = paste(pre, stat, gran, 'recent.csv', sep = '_'),
                from = max(from, cy_jan),
                to = min(to, yd12)
            )))
    }
    # add all previous 10 years
    if (from < cy_jan) {
        from_base10 <- floor(lubridate::year(from) / 10) * 10
        from_bases <- seq(from_base10, lubridate::year(cy_jan), by = 10)
        file_list <- c(file_list, lapply(from_bases, \(x) {
            list(
                filename = paste(pre, stat, gran, 
                    paste0('historical_', x, '-', x + 9, '.csv'), sep = '_'),
                from = max(from, lubridate::parse_date_time2(sprintf('%s-01-01', x), 
                        orders = '%Y-%m-%d', exact = TRUE)),
                to = min(to, cy_jan, lubridate::parse_date_time2(
                        sprintf('%s-01-01', x + 10), orders = '%Y-%m-%d', exact = TRUE))
            )
        }))
    }
    list(
        station = stat,
        granularity = gran,
        parameters = x[['parameter_shortname']],
        file_list = file_list
    )
}

# add function to get data
get_filenames <- function(meta_search) {
    # FIXME: => more than one collection! => loop recursively
    # meta_search = search_by_parameter(group = 'wind', granularity = 'T', meta_search = metadata[[7]])
    di <- meta_search$datainventory
    if (nrow(di) == 0) {
        return(list())
    }
    pa <- meta_search$parameters
    # prepare times
    now <- lubridate::with_tz(Sys.time(), tz = 'UTC')
    yesterday_12UTC <- as.POSIXct(trunc(now, 'days') - 12 * 3600)
    current_year_jan1 <- as.POSIXct(trunc(now, 'years'))
    from <- attr(meta_search, 'search_from')[1]
    if (is.null(from)) {
        from <- attr(meta_search, 'data_since')
    }
    to <- attr(meta_search, 'search_from')[2]
    if (is.null(to)) {
        to <- attr(meta_search, 'data_till')
    }
    # prepended filenames
    pre <- sub('ch.meteoschweiz.', '', collection <- attr(meta_search, 'collection'),
        fixed = TRUE)
    # loop over station/granularity pairs
    dat <- merge(di[, c('station_abbr', 'parameter_shortname', 'data_since', 'data_till')],
        pa[, c('parameter_shortname', 'parameter_granularity')])
    # split by groups of station/granularity
    dsplit <- split(dat, paste(dat$station_abbr, dat$parameter_granularity, sep = '/'))
    # loop over groups
    structure(c(
        list(collection = collection),
        lapply(dsplit, \(x) {
            .get_filenames(x, from, to, now, pre, yesterday_12UTC, current_year_jan1)
        })
    ), class = 'file_list')
}

# x1 <- search_by_parameter(group = 'wind', granularity = 'T', meta_search = metadata[[7]])
# xx <- get_filenames(x1)

get_files <- function(x, cache_dir = tempdir()) {
    # check if more than one collection
    # also check class
    # get collection
    cl <- x$collection
    # loope over file list
    structure(c(
        list(collection = cl),
        lapply(x[-1], \(l) {
            # get info on station
            info <- content(GET(ms_url('api/stac/v1/collections/', cl, '/items/', 
                        l$station)))$assets
            # download files
            c(
                l,
                files = {
                    out <- lapply(l$file_list, \(fl) {
                    # what if missing?
                    if (fl$filename %in% names(info)) {
                        dl_data(ms_url(cl, '/', l$station, '/', fl$filename), 
                            checksum = info[[fl$filename]][['file:checksum']], 
                            cache_dir = cache_dir)
                    } else {
                        warning('file "', fl$filename, '" cannot be downloaded')
                        NULL
                    }
                    })
                    names(out) <- sapply(l$file_list, '[[', 'filename')
                    list(out)
                }
            )
        })
    ), class = 'dl_files')
}

# yy <- get_files(xx[1:5])

# x <- yy[1:2]

# str(x)

# xy <- content(GET(ms_url('api/stac/v1/collections/', attr(meta_search, 'collection'), '/items/',
#         tolower(di[[1]][1]))))
# str(xy)
# names(xy$assets)

require(data.table)
get_data <- function(x, as_DT = TRUE) {
    # check class & check if more than one collection
    # loop over splits
    out <- lapply(x[-1], \(sp) {
        # time format
        time_format <- switch(sp$granularity
            , 'h' = 
            , 't' = '%d.%m.%Y %H:%M'
            , stop('fix current granularity in `get_data()`')
        )
        # loop over files
        d_list <- lapply(sp$file_list, \(fl) {
            suppressWarnings(
                dat <- fread(sp$files[[fl$filename]], select = c('station_abbr', 
                        'reference_timestamp', sp$parameters))
            )
            # parse times & subset
            dat[, time := lubridate::fast_strptime(reference_timestamp, 
                format = time_format, lt = FALSE)][, reference_timestamp := NULL]
            # subset
            dat[time >= fl$from & time <= fl$to]
        })
        dout <- rbindlist(d_list, fill = TRUE)
        # sort by time as first column
        setcolorder(dout, 'time')
        setorder(dout, 'time')
        # return
        dout
    })
    # return list
    if (!as_DT) {
        out <- lapply(out, as.data.frame)
    }
    out
}

if (FALSE) {
    # x1 <- search_by_parameter(group = 'wind', granularity = 'T', meta_search = metadata[[7]])
    # # TODO: pass parameter/station info down the stream
    # xx <- get_filenames(x1)
    # yy <- get_files(xx[1:5])
    # zz_data <- get_data(yy)

    # TODO: station_info(zz_data), parameter_info(zz_data)..

    # TODO: add granularity & parameter group to print.meta_search

    # x1 <- search_by_parameter(group = c('wind', 'temperature'), granularity = 'H', meta_search = metadata[[7]])
    # x1$parameter

    x1 <- search_by_parameter(shortname = c('fkl010h0', 'tre200h0'), granularity = 'H', meta_search = metadata[[7]])
    head(x1$stations[, 1:16])
    nrow(x1$stations)
    names(x1$stations)
    # qs2::qd_save(x1$stations[, 1:16], '~/repos/5_GitHub/agrammon-workbench/alfam2/idaweb-stations.qdata')

    x2 <- search_by_parameter(shortname = c('fkl010h0', 'tre200h0'), granularity = 'H', 
        meta_search = metadata[[7]])

    # Zollikofen
    # 2'601'931.15, 1'204'410.72
    # 46.990755, 7.464018
    xz <- search_by_location(x2, '7.43..7.49', '46.96..47.12')
    xx <- get_filenames(xz)
    yy <- get_files(xx)
    zz_data <- get_data(yy)
    # qs2::qd_save(zz_data[[1]], '~/repos/5_GitHub/agrammon-workbench/alfam2/zol-temp-ws.qdata')

}

## methods ----------------------------------------

# print method for collections
print.ms_collections <- function(x, ...) {
    # get titles
    titles <- sapply(attr(x, 'collections'), '[[', 'title')
    mt <- max(nchar(titles)) + grepl('[^a-zA-Z0-9:)( -]', titles) * 2
    cat('~~~~~~~~~~~~~~~~~~~~~~\n')
    cat('Open Data - MeteoSwiss\n\n')
    cat(nl <- length(x), 'collections available:\n')
    cat(paste(rep('-', nchar(as.character(nl))), collapse = ''),
        '-----------------------\n', sep = '')
    for (i in seq_len(nl)) {
        cat(sprintf(
                paste0('%2i: %-', mt[i], 's -> %s\n')
                , i, titles[i], x[i]))
    }
    cat('~~~~~~~~~~~~~~~~~~~~~~\n')
    invisible()
}

# print method for search results
print.meta_search <- function(x, ...) {
    # fix paras
    paras <- paste(attr(x, 'parameters'), collapse = ',')
    if (nchar(paras) > 40) {
        paras <- sub('^(.{10,20}[,]).+(,.{10,20})$', '\\1...\\2', paras)
    }
    # fix till
    data_till <- attr(x, 'data_till')
    if (is.na(data_till) && lubridate::is.POSIXct(data_till)) {
        data_till <- 'today'
    } else {
        data_till <- format(data_till)
    }
    cat('~~~\n')
    cat('Collection:', attr(x, 'collection'), '\n')
    cat('  data since', format(attr(x, 'data_since')), '\n')
    cat('  data until', data_till, '\n')
    cat('  wgs84 lat', paste(attr(x, 'wgs84_lat'), collapse = ' .. '), '\n')
    cat('  wgs84 lon', paste(attr(x, 'wgs84_lon'), collapse = ' .. '), '\n')
    cat('  parameters:', paras, '\n')
    cat('~~~\n')
    invisible()
}

##  • helper functions ====================

# helper function to download data
# and get path to local file
dl_data <- function(url, checksum = NULL, cache_dir = tempdir()) {
    # get data name
    data_name <- basename(url)
    # check if data is already available locally
    local_file <- getOption(data_name)
    # check if cache_dir matches
    if (is.null(local_file)) {
        # check if directory exists
        if (!file.exists(cache_dir)) {
            stop('directory "', cache_dir, '" does not exist!')
        }
        # temporary file path
        local_file <- file.path(cache_dir, data_name)
        # check if file exists nevertheless
        if (!file.exists(local_file)) {
            # download file
            dl_code <- download.file(url = url, destfile = local_file)
            if (dl_code != 0L) {
                stop('Download of file "', url, '" failed with exit code ', dl_code, 
                    call. = FALSE)
            }
        }
        # check checksum if available
        if (!is.null(checksum)) {
            if (!grepl('^1220', checksum)) {
                stop('dubious checksum start "1220" has been changed or removed -> FIX ME')
            }
            # get checksum
            if (sub('^1220', '', checksum) != 
                as.character(openssl::sha256(file(local_file)))) {
                warning('checksum of file "', local_file ,
                    '" does not match provided "file:checksum"!')
            }
        }
        # add path to options
        options(setNames(list(local_file), data_name))
    } else if (!grepl(cache_dir, local_file, fixed = TRUE)) {
        warning('file: "', data_name, '" has already been downloaded to "', 
            dirname(local_file), '".\n -> ignoring cache_dir argument.')
    }
    # return path
    invisible(local_file)
}

# check if supported
check_supported <- function(id) {
    if (length(id) == 1L && is.character(id)) {
        sc <- supported_collections()
        i <- which(sc %in% id)
        if (length(i) == 0L) {
            structure(FALSE, collection = list())
        } else {
            structure(TRUE, collection = attr(sc, 'collections')[[i]])
        }
    } else if (length(id) > 1L) {
        out <- lapply(id, check_supported)
        structure(
            unlist(out),
            collection = lapply(out, attr, 'collection')
        )
    } else {
        FALSE
    }
}

# parse from & to datetime input
check_fromto <- function(from, to, tz = get_tzone(from, to)) {
    if (!missing(from) && length(from) != 1L) stop('argument "from" must have length 1!')
    if (!missing(to) && length(to) != 1L) stop('argument "to" must have length 1!')
    if (missing(to)) {
        # check from
        switch(class(from)[1]
            , character = {
                seps <- c('to', '/', '::', ' - ')
                # split any time ranges
                from_to <- trimws(unlist(strsplit(from, 
                    split = paste0(' ?', paste(seps, collapse = ' ?| ?'), ' ?'))))
                # parse datetimes to POSIXct
                if (from_to[1] == '') {
                    from <- as.POSIXct(-Inf)
                } else {
                    from <- fa_st(from_to[1], tz = tz)
                }
                if (length(from_to) == 1L) {
                    to <- as.POSIXct(Inf)
                } else {
                    to <- fa_st(from_to[2], tz = tz)
                }
            }
            , POSIXlt = {
                from <- as.POSIXct(from)
                to <- as.POSIXct(Inf)
            }
            , POSIXct = {
                to <- as.POSIXct(Inf)
            }
            , stop('argument "from" should be of class "character" or "POSIXt"!')
        )
    } else if (missing(from)) {
        # parse to
        to <- switch(class(to)[1]
            , character = fa_st(trimws(to), tz = tz)
            , POSIXlt = as.POSIXct(to)
            , POSIXct = to
            , stop('argument "to" should be of class "character" or "POSIXt"!')
        )
        from <- as.POSIXct(-Inf)
    } else {
        # parse from
        from <- switch(class(from)[1]
            , character = fa_st(trimws(from), tz = tz)
            , POSIXlt = as.POSIXct(from)
            , POSIXct = from
            , stop('argument "from" should be of class "character" or "POSIXt"!')
        )
        # parse to
        to <- switch(class(to)[1]
            , character = fa_st(trimws(to), tz = tz)
            , POSIXlt = as.POSIXct(to)
            , POSIXct = to
            , stop('argument "to" should be of class "character" or "POSIXt"!')
        )
    }
    # check if to > from
    if (to <= from) {
        stop('argument "to" must indicate a time which occurs later than "from"')
    }
    # return vector
    c(from, to)
}

# parse time zone with default UTC
get_tzone <- function(x, y) {
    if (missing(x)) {
        x <- y
    }
    tzx <- attr(x, 'tzone')
    if (missing(y)) {
        y <- x
    }
    tzy <- attr(y, 'tzone')
    if (is.null(tzx) && is.null(tzy)) {
        if (inherits(x, 'POSIXt') || inherits(y, 'POSIXt')) {
            tzout <- ''
        } else {
            tzout <- 'UTC'
        }
    } else if (!is.null(tzx)) {
        tzout <- tzx
    } else {
        tzout <- tzy
    }
    tzout
}

# parse characters with common datetime formats
fa_st <- function(x, tz) {
    formats <- c("%Y", "%d.%m.%Y", "%d.%m.%y", "%d.%m.%Y %H:%M", "%d.%m.%y %H:%M",
        "%d.%m.%Y %H:%M:%S", "%d.%m.%y %H:%M:%S", "%Y-%m-%d", "%y-%m-%d", 
        "%Y-%m-%d %H:%M", "%y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S", "%y-%m-%d %H:%M:%S")
    lubridate::fast_strptime(x, format = formats, tz = tz, lt = FALSE)
}

# "fuzzy" searching strings
fuzzy_search <- function(x, y, ignore.case = TRUE, value = FALSE) {
    fuzzy_x <- paste(c('', unlist(strsplit(x, split = '')), ''), collapse = '.*')
    grep(fuzzy_x, y, value = value, ignore.case = ignore.case)
}


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
