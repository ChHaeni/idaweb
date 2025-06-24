


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

search_by_datetime <- function(from, to, tz = get_tzone(from, to), ms_search = metadata) {
    # change argument ms_search to ms_search = idaweb:::metadata or similar argument name
    # parse from & to
    fromto <- check_fromto(from, to, tz = tz)
    # select datainventory/station/parameters
    if ('datainventory' %in% names(ms_search)) {
        if (!is.null(sft <- attr(ms_search, 'search_fromto'))) {
            # check from
            fromto[1] <- max(sft[1], fromto[1])
            # check to
            fromto[2] <- min(sft[2], fromto[2])
        }
        # TODO: improve these if/else tests! and capture errors
        # check from
        i_from <- is.na(ms_search$datainventory$data_till) | 
            fromto[1] <= ms_search$datainventory$data_till
        # check to
        i_to <- i_from & ms_search$datainventory$data_since <= fromto[2]
        # return subset incl from/to
        sub_inv <- ms_search$datainventory[i_to, ]
        # get stations
        sub_stats <- ms_search$stations[ms_search$stations$station_abbr %in% 
            sub_inv$station_abbr, ]
        # get parameters
        sub_paras <- ms_search$parameters[ms_search$parameters$parameter_shortname %in% 
            sub_inv$parameter_shortname, ]
        structure(
            list(
                assets = ms_search$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'ms_search', 
            # get since & till
            data_since = min(sub_inv$data_since),
            data_till = max(sub_inv$data_till),
            wgs84_lat = range(sub_stats$station_coordinates_wgs84_lat),
            wgs84_lon = range(sub_stats$station_coordinates_wgs84_lon),
            parameters = unique(sub_paras$parameter_shortname),
            collection = basename(dirname(ms_search$assets[[1]]$href)),
            search_fromto = fromto,
            search_location = attr(ms_search, 'search_location'),
            search_parameters = attr(ms_search, 'search_parameters')
        )
    } else {
        sapply(ms_search, search_by_datetime, from = fromto[1], to = fromto[2], tz = tz,
            simplify = FALSE)
    }
}

# # zz1 <- search_by_datetime('01.01.2018 to 05.02.2018', ms_search = metadata[10])
# zz1 <- search_by_datetime('01.01.2018 to 05.02.2018', ms_search = metadata[7])
# zz1b <- search_by_datetime('01.01.2017 to 01.02.2018', ms_search = zz1)
# zz2 <- search_by_datetime('01.01.2018 to 05.02.2018', ms_search = metadata[[7]])
# search_by_datetime('01.01.2018 to 05.02.2018', ms_search = metadata)
# search_by_datetime('13.08.2020')
# search_by_datetime('07.02.2024/08.03.2025')
# search_by_datetime(to = '13.08.2020')
# search_by_datetime(from = '01.01.2018', to = '13.08.2020')

search_by_location
# -> search ms_search$stations
# TODO:
#   allow searching by both lv95 & wgs84, even lv03?
#   => convert between coordinate systems -> use sf?
sf::st_crs('EPSG:4326')
sf::st_crs('EPSG:2056')
sf::st_crs('EPSG:21781')
x <- cbind(c(600000, 620000), c(200000, 220000))
x1 <- gel::set_crs(x, 'lv03')
x2 <- gel::set_crs(x, 'lv95')
sf::sf_project('EPSG:21781', 'EPSG:4326', x)

search_by_parameter <- function(shortname, unit, group, description, 
    language = c('en', 'de', 'fr', 'it'), 
    granularity = c('T', 'H', 'D', 'M', 'Y'), 
    ms_search = metadata) {
    if ('datainventory' %in% names(ms_search)) {
        sub_paras <- ms_search$parameter
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
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(granularity = granularity))
        }
        if (!missing(description)) {
            # search by group (vector)
            ind <- unlist(lapply(group, fuzzy_search, 
                sub_paras[[paste0('parameter_description_', language)]]))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(description = description))
        }
        # get inventory
        sub_inv <- ms_search$datainventory[ms_search$datainventory$parameter_shortname %in% 
            sub_paras$parameter_shortname, ]
        # get stations
        sub_stats <- ms_search$stations[ms_search$stations$station_abbr %in% 
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
                assets = ms_search$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'ms_search', 
            # get since & till
            data_since = data_since,
            data_till = data_till,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            collection = basename(dirname(ms_search$assets[[1]]$href)),
            search_fromto = attr(ms_search, 'search_fromto'),
            search_location = attr(ms_search, 'search_location'),
            search_parameters = search_parameters
        )
    } else {
        # mc <- match.call(
        # sapply(ms_search, \(x, ...) {
        #     do.call(search_by_parameter, c(list(ms_search = x), ...))
        # }, as.list(mc)[-1], simplify = FALSE)
        sapply(ms_search, search_by_parameter, shortname = shortname, unit = unit, 
            group = group, description = description, language = language, 
            granularity = granularity, simplify = FALSE)
    }
}


xx <- search_by_parameter(group = 'wind', granularity = 'T')
x1 <- search_by_parameter(group = 'wind', granularity = 'T', ms_search = metadata[[1]])

# -> search ms_search$parameters
# -> search/filter by shortname, units, granularity, group, description in all languages(?)
#   add option to choose language with _en as default
head(metadata[[1]]$parameters)
str(metadata[[1]]$parameters)
x <- 'wind hom jahrmitt'
y <- paste(c('', unlist(strsplit(x, split = '')), ''), collapse = '.*')
grep(y, metadata[[1]]$parameters[, 2], value = TRUE, ignore.case = TRUE)
x <- 'wind speed monthly mean'
y <- paste(c('', unlist(strsplit(x, split = '')), ''), collapse = '.*')
grep(y, metadata[[1]]$parameters[, 'parameter_description_en'], value = TRUE, ignore.case = TRUE)
x <- 'wind speed monthly mean'
fuzzy_search(x, metadata[[1]]$parameters[, 'parameter_description_en'])

fuzzy_search <- function(x, y, ignore.case = TRUE) {
    fuzzy_x <- paste(c('', unlist(strsplit(x, split = '')), ''), collapse = '.*')
    grep(fuzzy_x, y, value = TRUE, ignore.case = ignore.case)
}

# add option to provide previous results for further subsetting
# add function to bind different results together
# add function to get data from results

# -> convenience functions => show_stations, show_parameters

# add function to get data


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
print.ms_search <- function(x, ...) {
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
    if (is.null(local_file)) {
        # temporary file path
        local_file <- tempfile(pattern = 'cached_', fileext = data_name, tmpdir = cache_dir)
        # download file
        dl_code <- download.file(url = url, destfile = local_file)
        if (dl_code != 0L) {
            stop('Download of file "', url, '" failed with exit code ', dl_code, 
                call. = FALSE)
        }
        # check checksum if available
        if (!is.null(checksum)) {
            if (!grepl('^1220', checksum)) {
                stop('dubious checksum start "1220" has been changed or removed -> FIX ME')
            }
            # get checksum
            if (sub('^1220', '', checksum) != 
                as.character(openssl::sha256(file(local_file)))) {
                stop('checksum does not match provided "file:checksum"!')
            }
        }
        # add path to options
        options(setNames(list(local_file), data_name))
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
