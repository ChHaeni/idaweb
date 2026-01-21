


## NOTES ----------------------------------------

# REST API docu
# https://data.geo.admin.ch/api/stac/static/spec/v1/api.html#tag/Data

# only collections from meteoswiss
# only ids containing ogd (because of meta data & data format)
# check data sets: https://opendatadocs.meteoswiss.ch/

# ground-base measurements only
# data sets A1 to A9
# smn, smn-preicp, smn-tower, nime, tot, pollen, obs, phenology

# note on data handling:
# mail from support: Re: Incident INC000002367960 / Daten 2025 unter "aktuelles Jahr"
#   Die Daten 2025 sind bis im Februar noch unter «aktuelles Jahr» zu finden.
#   Danach werden die Daten geprüft und definitiv und unter 2020-2929 auffindbar sein.

# overview on functions:
# -

# TODO:
# - fetch data info
# - meta data included in package -> check if update needed
# - function to update specific or all meta data (if necessary)
# - download data only if not available in options
# - one function to get data (incl. options check)
# - show_on_map() => visualize subset on map
# - convenience functions:
#   * get_data -> rename current get_data to .get_data & wrap get_filenames, get_files &
#       .get_data into one function
#   * search_data -> wrapper for all search_by_* functions
#   * parameters(), stations(), datainventory()

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
collections <- function(set_name = NULL) {
    if (is.list(set_name)) {
        set_names <- sub('.*\\.ogd-', '', names(set_name))
    } else if (!is.null(set_name) && is.character(set_name)) {
        set_names <- set_name
    } else {
        # all ground-base measurement sets
        set_names <- c('smn', 'smn-precip', 'smn-tower', 'nime', 'tot', 
            'pollen', 'obs', 'phenology')
    }
    # loop over sets
    out <- lapply(set_names, \(x) {
        # fix url
        get_url <- ms_url('api/stac/v1/collections/ch.meteoschweiz.ogd-', x)
        # get info
        content(GET(get_url))
    })
    # get ids
    ids <- sapply(out, '[[', 'id')
    attr(ids, 'collections') <- out
    structure(ids, class = 'ms_collections')
}

# function to get info on specific data set(s) from collection
# e.g.: info(collections())
# x -> either ms_collections object or a valid id
info <- function(x, i = NULL) {
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
            # load('data/metadata.rda')
            meta_last_updated <- lubridate::fast_strptime(
                # idaweb:::metadata[[id]]$assets[[file_name]]$updated,
                # replace me when package
                metadata[[id]]$assets[[file_name]]$updated,
                format = "%Y-%m-%dT%H:%M:%OSZ", lt = FALSE
            )
            if (last_updated > meta_last_updated) {
                # update data...
                # get url & checksum
                file_url <- id_coll$assets[[file_name]]$href
                file_checksum <- id_coll$assets[[file_name]][['file:checksum']]
                if (!(basename(file_url) %in% names(.Options))) {
                    # TODO: add message about github issue (only first time if local
                    #           file does not exist yet)
                    # cat()
                    warning(file_name, ' has recent changes: metadata.rda in package needs updating!')
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
            nm <- sub('.+_meta_(.+)[.]csv', '\\1', file_name)
            # return(idaweb:::metadata[[id]][[nm]])
            # replace me when package
            return(metadata[[id]][[nm]])
        } else {
            return(invisible(NULL))
        }
    } else {
        # get data
        out <- lapply(id, get_metadata, type = type, cache_dir = cache_dir)
        names(out) <- id
        # remove invalid
        return(out[!sapply(out, is.null)])
    }
}

parameters <- function(meta_data, cols = NULL, uniq = !is.null(cols)) {
    if (inherits(meta_data, 'ms_metadata')) {
        out <- meta_data$parameters
        if (!is.null(cols)) {
            out <- out[, cols]
            if (uniq) {
                out <- unique(out)
            }
        }
    } else if (is.list(meta_data)) {
        out <- sapply(meta_data, parameters, simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
    out
}
stations <- function(meta_data) {
    if (inherits(meta_data, 'ms_metadata')) {
        meta_data$stations
    } else if (is.list(meta_data)) {
        sapply(meta_data, stations, simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
}
datainventory <- function(meta_data) {
    if (inherits(meta_data, 'ms_metadata')) {
        meta_data$datainventory
    } else if (is.list(meta_data)) {
        sapply(meta_data, datainventory, simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
}

## hier weiter!!!
# TODO:
#   search functions
#   search by: time range, location range, parameters (fuzzy search)

fix_meta_arg <- function(meta) {
    meta_arg <- deparse(substitute(meta))
    if (missing(meta)) {
        # change once package
        # return(idaweb:::metadata)
        return(metadata)
    } else if (is.character(meta)) {
        # change once package
        # ind <- sub('ch.meteoschweiz.ogd-', '', names(idaweb:::metadata)) %in%
        ind <- sub('ch.meteoschweiz.ogd-', '', names(metadata)) %in%
            sub('ch.meteoschweiz.ogd-', '', meta)
        if (any(ind)) {
            # collection name(s)
            if (length(meta) == 1L) {
                meta <- metadata[[which(ind)]]
            } else {
                meta <- metadata[ind]
            }
        } else {
            meta <- switch(meta
                # 'all' = idaweb:::metadata,
                , 'all' = metadata
                # any others?
                # error unknown
                , stop('cannot interpret ',meta_arg ,' argument!', call. = FALSE)
            )
        }
        return(meta)
    } else if (inherits(meta, 'ms_metadata')) {
        # meta data of single collection
        return(meta)
    } else if (is.list(meta)) {
        # check list of meta data
        check_list <- unlist(lapply(meta, inherits, 'ms_metadata'))
        if (all(check_list)) {
            # list of collections
            return(meta)
        }
    }
    # throw error (TODO: add parent.call via argument)
    stop('cannot interpret ', meta_arg , ' argument!', call. = FALSE)
}

search_by_datetime <- function(from, to, tz = get_tzone(from, to), meta_data = metadata, drop_nodata = FALSE) {
    # change argument meta_data to meta_data = idaweb:::metadata or similar argument name
    # fix meta argument
    meta_data <- fix_meta_arg(meta_data)
    # parse from & to
    fromto <- check_fromto(from, to, tz = tz)
    # select datainventory/station/parameters
    if ('datainventory' %in% names(meta_data)) {
        if (!is.null(sft <- attr(meta_data, 'search_fromto'))) {
            # check from
            fromto[1] <- max(sft[1], fromto[1])
            # check to
            fromto[2] <- min(sft[2], fromto[2])
        }
        # TODO: improve these if/else tests! and capture errors
        # check from
        i_from <- is.na(meta_data$datainventory$data_till) | 
            fromto[1] <= meta_data$datainventory$data_till
        # check to
        i_to <- i_from & meta_data$datainventory$data_since <= fromto[2]
        # return subset incl from/to
        sub_inv <- meta_data$datainventory[i_to, ]
        if (drop_nodata && nrow(sub_inv) == 0) {
            return(NULL)
        }
        # get stations
        sub_stats <- meta_data$stations[meta_data$stations$station_abbr %in% 
            sub_inv$station_abbr, ]
        # get parameters
        sub_paras <- meta_data$parameters[meta_data$parameters$parameter_shortname %in% 
            sub_inv$parameter_shortname, ]
        if (nrow(sub_inv) > 0) {
            data_since <- min(sub_inv$data_since)
            data_till <- max(sub_inv$data_till)
            wgs84_lat <- range(sub_stats$station_coordinates_wgs84_lat)
            wgs84_lon <- range(sub_stats$station_coordinates_wgs84_lon)
            parameters <- unique(sub_paras$parameter_shortname)
            stations <- unique(sub_stats$station_abbr)
        } else {
            data_since <- lubridate::NA_POSIXct_
            data_till <- NA_integer_
            wgs84_lat <- c(NA_real_, NA_real_)
            wgs84_lon <- c(NA_real_, NA_real_)
            parameters <- NA_character_
            stations <- NA_character_
        }
        structure(
            list(
                assets = meta_data$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'ms_metadata', 
            # pass collection
            collection = attr(meta_data, 'collection'),
            # update further attributes
            stations = stations,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            data_since = data_since,
            data_till = data_till,
            search_fromto = fromto,
            search_location = attr(meta_data, 'search_location'),
            search_parameters = attr(meta_data, 'search_parameters')
        )
    } else {
        out <- sapply(meta_data, search_by_datetime, from = fromto[1], 
            to = fromto[2], tz = tz, drop_nodata = drop_nodata,
            simplify = FALSE)
        if (drop_nodata) {
            out[!sapply(out, is.null)]
        } else {
            out
        }
    }
}

# TODO: add z (height above sea level)
# TODO: add search_by_station function (station abbr, name, canton, ...)
search_by_location <- function(x, y, z = NULL, station_abbr = NULL,
    station_name = NULL, station_canton = NULL, meta_data = metadata, 
    drop_nodata = FALSE) {
    # valid search entries:
    # lat & lon: '46.1..46.2', '46.1 to 46.2', '46.1/46.2', c(46.1, 46.2), 
    # ch_x & ch_y: same as above BUT additionally, only 100-thousands 
    #   -> distinguish between lv95 and lv03
    #   -> check x/y as required in R and possibly flip
    # only attach sf if really necessary
    # fix meta argument
    meta_data <- fix_meta_arg(meta_data)
    # change argument meta_data to meta_data = idaweb:::metadata or similar argument name
    # parse x
    xv <- check_xy_arg(x)
    # parse y
    yv <- check_xy_arg(y)
    # parse z
    zv <- check_z_arg(z)
    # select datainventory/station/parameters
    if ('datainventory' %in% names(meta_data)) {
        if (!is.null(sft <- attr(meta_data, 'search_location'))) {
            stop('Fix already searched by location')
        }
        # get number of stations
        n_stations <- nrow(meta_data$stations)
        # TODO: improve these if/else tests! and capture errors
        # check x/lon
        if (is.null(xv)) {
            i_x <- rep(TRUE, n_stations)
        } else {
            # subset by longitude
            s_lon <- meta_data$stations$station_coordinates_wgs84_lon 
            i_x <- unlist(lapply(xv, \(v) s_lon >= v[1] & s_lon <= v[2]))
        }
        # check y/lat
        if (is.null(yv)) {
            i_y <- rep(TRUE, n_stations)
        } else {
            # subset by latitude
            s_lat <- meta_data$stations$station_coordinates_wgs84_lat 
            i_y <- unlist(lapply(yv, \(v) s_lat >= v[1] & s_lat <= v[2]))
        }
        # check z/elevation
        if (is.null(zv)) {
            i_z <- rep(TRUE, n_stations)
        } else {
            # subset by elevation
            s_el <- meta_data$stations$station_height_masl
            i_z <- unlist(lapply(zv, \(v) s_el >= v[1] & s_el <= v[2]))
        }
        # check station_abbr
        if (is.null(station_abbr)) {
            i_abbr <- rep(TRUE, n_stations)
        } else {
            i_abbr <- meta_data$stations$station_abbr %in% station_abbr
        }
        # check station_name
        if (is.null(station_name)) {
            i_name <- rep(TRUE, n_stations)
        } else {
            i_name <- unlist(lapply(station_name, fuzzy_search, 
                meta_data$stations$station_name, return_logical = TRUE))
        }
        # check station_canton
        if (is.null(station_canton)) {
            i_canton <- rep(TRUE, n_stations)
        } else {
            i_canton <- meta_data$stations$station_canton %in% station_canton
        }
        # combine all
        i_ok <- i_x & i_y & i_z & i_abbr & i_name & i_canton
        # return subset of stations
        sub_stats <- meta_data$stations[i_ok, ]
        # get inventory
        sub_inv <- meta_data$datainventory[meta_data$datainventory$station_abbr 
            %in% sub_stats$station_abbr, ]
        if (drop_nodata && nrow(sub_inv) == 0) {
            return(NULL)
        }
        # get parameters
        sub_paras <- meta_data$parameters[meta_data$parameters$parameter_shortname %in% 
            sub_inv$parameter_shortname, ]
        if (nrow(sub_inv) > 0) {
            data_since <- min(sub_inv$data_since)
            data_till <- max(sub_inv$data_till)
            wgs84_lat <- range(sub_stats$station_coordinates_wgs84_lat)
            wgs84_lon <- range(sub_stats$station_coordinates_wgs84_lon)
            parameters <- unique(sub_paras$parameter_shortname)
            stations <- unique(sub_stats$station_abbr)
        } else {
            data_since <- lubridate::NA_POSIXct_
            data_till <- NA_integer_
            wgs84_lat <- c(NA_real_, NA_real_)
            wgs84_lon <- c(NA_real_, NA_real_)
            parameters <- NA_character_
            stations <- NA_character_
        }
        structure(
            list(
                assets = meta_data$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'ms_metadata', 
            # pass collection
            collection = attr(meta_data, 'collection'),
            # update further attributes
            stations = stations,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            data_since = data_since,
            data_till = data_till,
            search_fromto = attr(meta_data, 'search_fromto'),
            search_location = list(x = x, y = y, z = z, station_abbr = station_abbr,
                station_name = station_name, station_canton = station_canton),
            search_parameters = attr(meta_data, 'search_parameters')
        )
    } else {
        out <- sapply(meta_data, search_by_location, x = xv, y = yv, 
            z = zv, station_abbr = station_abbr, station_name = station_name,
            station_canton = station_canton, drop_nodata = drop_nodata, 
            simplify = FALSE
        )
        if (drop_nodata) {
            out[!sapply(out, is.null)]
        } else {
            out
        }
    }
}

check_xy_arg <- function(xy) {
    if (missing(xy) || is.null(xy)) {
        return(NULL)
    }
    xy_nm <- deparse(substitute(xy))
    if (is.list(xy) && all(sapply(xy, is.numeric)) &&
        unique(lengths(xy)) == 2L) {
        return(xy)
    }
    if (is.numeric(xy)) {
        if (length(xy) != 2) {
            stop('if argument', xy_nm, 'is numeric, length must be 2 (use -Inf/Inf for open limits)')
        }
        v_out <- list(xy)
    } else {
        # define valid separators
        seps <- c('[.][.]', 'to', '/', '//', '-')
        # check list format
        if (is.list(xy)) {
            stop('Fix list input!')
            ul <- unique(lengths(xy))
            if (length(ul) > 1 || ul < 1 || ul > 2) {
                stop('list input not valid')
            }
        }
        # check separators
        xyl <- strsplit(xy, split = paste(seps, collapse = '|'))
        # get limits
        switch(xy_nm
            , x = {
                wgs_lims <- c(4, 12)
            }
            , y = {
                wgs_lims <- c(42, 50)
            }
        )
        # fix coord values
        v_out <- lapply(xyl, \(z) {
            v <- as.numeric(z)
            if (all(v > wgs_lims[1] & v < wgs_lims[2])) {
                # lon
                # v is ok
            } else {
                stop('Fix ch coordinates')
                # 200/200000, 1200/1200000
            }
            v
        })
    }
    # return list of values
    v_out
}

check_z_arg <- function(z) {
    if (missing(z) || is.null(z)) {
        return(NULL)
    }
    z_nm <- deparse(substitute(z))
    if (is.list(z) && all(sapply(z, is.numeric)) &&
        unique(lengths(z)) == 2L) {
        return(z)
    }
    if (is.numeric(z)) {
        if (length(z) != 2) {
            stop('if argument', z_nm, 'is numeric, length must be 2 (use -Inf/Inf for open limits)')
        }
        v_out <- list(z)
    } else {
        # define valid separators
        seps <- paste0('\\s*', c('[.][.]', 'to', '/', '//', '-'), '\\s*')
        # check list format
        if (is.list(z)) {
            stop('Fix list input!')
            ul <- unique(lengths(z))
            if (length(ul) > 1 || ul < 1 || ul > 2) {
                stop('list input not valid')
            }
        }
        # split by separators
        zsp <- strsplit(z, split = paste(seps, collapse = '|'))
        v_out <- lapply(zsp, \(x) {
            if (length(x) == 1) {
                if (grepl('>=?', x)) {
                    c(as.numeric(sub('\\s*>=?\\s*', '', x)), Inf)
                } else if (grepl('<=?', x)) {
                    c(0, as.numeric(sub('\\s*<=?\\s*', '', x)))
                } else {
                    c(as.numeric(x), Inf)
                }
            } else if (length(x) > 2) {
                stop('argument', z_nm, 'is not in a recognized format!')
            } else {
                x <- as.numeric(x)
                if (is.na(x[1])) x[1] <- 0
                x
            }
        })
    }
    # return list of values
    v_out
}

search_by_parameter <- function(shortname, unit, group, description, 
    language = c('en', 'de', 'fr', 'it'), 
    granularity = c('T', 'H', 'D', 'M', 'Y'), 
    meta_data = metadata, drop_nodata = FALSE) {
    # fix meta argument
    meta_data <- fix_meta_arg(meta_data)
    if ('datainventory' %in% names(meta_data)) {
        sub_paras <- meta_data$parameter
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
        sub_inv <- meta_data$datainventory[
            meta_data$datainventory$parameter_shortname %in% 
            sub_paras$parameter_shortname, ]
        if (drop_nodata && nrow(sub_inv) == 0) {
            return(NULL)
        }
        # get stations
        sub_stats <- meta_data$stations[meta_data$stations$station_abbr %in% 
            sub_inv$station_abbr, ]
        if (nrow(sub_inv) > 0) {
            data_since <- min(sub_inv$data_since)
            data_till <- max(sub_inv$data_till)
            wgs84_lat <- range(sub_stats$station_coordinates_wgs84_lat)
            wgs84_lon <- range(sub_stats$station_coordinates_wgs84_lon)
            parameters <- unique(sub_paras$parameter_shortname)
            stations <- unique(sub_stats$station_abbr)
        } else {
            data_since <- lubridate::NA_POSIXct_
            data_till <- NA_integer_
            wgs84_lat <- c(NA_real_, NA_real_)
            wgs84_lon <- c(NA_real_, NA_real_)
            parameters <- NA_character_
            stations <- NA_character_
        }
        structure(
            list(
                assets = meta_data$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'ms_metadata', 
            # pass collection
            collection = attr(meta_data, 'collection'),
            # update further attributes
            stations = stations,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            data_since = data_since,
            data_till = data_till,
            search_fromto = attr(meta_data, 'search_fromto'),
            search_location = attr(meta_data, 'search_location'),
            search_parameters = search_parameters
        )
    } else {
        out <- sapply(meta_data, search_by_parameter, shortname = shortname, 
            unit = unit, group = group, description = description, 
            language = language, granularity = granularity, 
            drop_nodata = drop_nodata, simplify = FALSE)
        if (drop_nodata) {
            out[!sapply(out, is.null)]
        } else {
            out
        }
    }
}

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
        if (from <= now && to > yd12) {
            file_list <- c(file_list, list(list(
                    filename = paste(pre, stat, gran, 'now.csv', sep = '_'),
                    from = max(from, yd12),
                    to = min(to, now)
                )))
        } 
    }
    # add previous year cut
    cy_jan_minus_1y <- cy_jan
    year(cy_jan_minus_1y) <- year(cy_jan) - 1
    yd_midnight <- yd12
    hour(yd_midnight) <- 23
    second(yd_midnight) <- minute(yd_midnight) <- 59
    if (from <= cy_jan && to > cy_jan) {
        file_list <- c(file_list, list(list(
                filename = paste(pre, stat, gran, 'recent.csv', sep = '_'),
                from = max(from, cy_jan),
                to = min(to, yd_midnight)
            )))
    } 
    if (from < yd12 && to > cy_jan_minus_1y && month(now) <= 2) {
        # previous year is included in current year until February (see mail support)
        if ((l <- length(file_list)) > 0 && grepl('recent', file_list[[l]]$filename)) {
            # update from & to, only
            file_list[[l]]$from <- max(from, cy_jan_minus_1y) 
            file_list[[l]]$to <- min(to, yd_midnight)
        } else {
            # add new entry
            file_list <- c(file_list, list(list(
                    filename = paste(pre, stat, gran, 'recent.csv', sep = '_'),
                    from = max(from, cy_jan_minus_1y),
                    to = min(to, yd_midnight)
                )))
        }
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
get_filenames <- function(meta_data) {
    # FIXME: => more than one collection! => loop recursively
    # meta_data = search_by_parameter(group = 'wind', granularity = 'T', meta_data = metadata[[7]])
    di <- meta_data$datainventory
    if (nrow(di) == 0) {
        return(list())
    }
    pa <- meta_data$parameters
    # prepare times
    now <- lubridate::with_tz(Sys.time(), tz = 'UTC')
    yesterday_12UTC <- as.POSIXct(trunc(now, 'days') - 12 * 3600)
    current_year_jan1 <- as.POSIXct(trunc(now, 'years'))
    from <- attr(meta_data, 'search_from')[1]
    if (is.null(from)) {
        from <- attr(meta_data, 'data_since')
    }
    to <- attr(meta_data, 'search_from')[2]
    if (is.null(to)) {
        to <- attr(meta_data, 'data_till')
    }
    # prepended filenames
    pre <- sub('ch.meteoschweiz.', '', collection <- attr(meta_data, 'collection'),
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

# x1 <- search_by_parameter(group = 'wind', granularity = 'T', meta_data = metadata[[7]])
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

# xy <- content(GET(ms_url('api/stac/v1/collections/', attr(meta_data, 'collection'), '/items/',
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


## methods ----------------------------------------

# print method for collections
print.ms_collections <- function(x, ...) {
    # get titles
    titles <- sapply(attr(x, 'collections'), '[[', 'title')
    mt <- max(nchar(titles)) + grepl('[^a-zA-Z0-9:)( -]', titles) * 2
    cat('~~~~~~~~~~~~~~~~~~~~~~\n')
    cat('Open Data - MeteoSwiss\n')
    cat('(Ground-based measurements)\n')
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

# print methods for meta data
print.ms_assets <- function(x, ...) {
    ncs <- nchar(nms <- names(x))
    names(ncs) <- nms
    cat('-- assets --\n')
    for (nm in nms) {
        sadd <- paste(rep(' ', max(ncs) - ncs[nm] + 1), collapse = '')
        cat(nm, sadd, 'last updated:', x[[nm]][['updated']], '\n')
    }
    invisible()
}

print.ms_datainventory <- function(x, ...) {
    cat('-- datainventory --\n')
    cat('Number of data:', length(x[[1]]), '\n')
    cat('Number of stations: ', length(unique(x[[1]])), '\n')
    cat('Number of parameters:', length(unique(x[[2]])), '\n')
    print_dense(x, ...)
}

print.ms_stations <- function(x, ...) {
    cat('-- stations --\n')
    cat('Number of stations:', length(x[[1]]), '\n')
    print_dense(x, ...)
}

print.ms_parameters <- function(x, ...) {
    cat('-- parameters --\n')
    cat('Number of parameters:', length(x[[1]]), '\n')
    cat('Granularities:', unique(x[['parameter_granularity']]), '\n')
    print_dense(x, ...)
}

print.ms_metadata <- function(x, ...) {
    # shorten parameter groups
    groups <- paste(unique(x[['parameters']][['parameter_group_en']]), collapse = ',')
    if (nchar(groups) > 40) {
        groups <- sub('^(.{10,20}[,]).+(,.{10,20})$', '\\1...\\2', groups)
    }
    # shorten stations
    stations <- paste(unique(x[['stations']][['station_abbr']]), collapse = ',')
    if (nchar(stations) > 40) {
        stations <- sub('^(.{10,20}[,]).+(,.{10,20})$', '\\1...\\2', stations)
    }
    # fix till
    data_till <- attr(x, 'data_till')
    if (is.na(data_till) && lubridate::is.POSIXct(data_till)) {
        data_till <- 'today'
    } else {
        data_till <- format(data_till)
    }
    # fix lon 
    # lon <- sub('^0', ' ', sprintf('%09.6f', attr(x, 'wgs84_lon')))
    # show only 3 digits (1e-3° ≈ 100 m)
    if (any(is.na(attr(x, 'wgs84_lon')))) {
        lon <- lat <- NA_character_
    } else {
        lon <- sub('^0', ' ', sprintf('%06.3f', attr(x, 'wgs84_lon')))
        lat <- sprintf('%06.3f', attr(x, 'wgs84_lat'))
    }
    # get collection info
    col <- attr(x, 'collection')
    # cat('~~~\n')
    cat('~~~ meta data ~~~\n')
    cat('Collection:', col, '\n')
    cat('**', attr(col, 'title'), '**\n')
    ncs <- nchar(nms <- names(x[['assets']]))
    names(ncs) <- nms
    # fix order: stat, para, data
    nms <- nms[order(ncs)]
    for (nm in nms) {
        sadd <- paste(rep(' ', max(ncs) - ncs[nm]), collapse = '')
        dnm <- sub('.+_meta_(.+)[.]csv', '\\1', nm)
        ladd <- paste0('(', nrow(x[[dnm]]), ' ', sub('inventory', '', dnm), ';')
        cat(' -', dnm, sadd, ladd, 'last updated', 
            sub('T.+', ')', x[[1]][[nm]][['updated']]), '\n')
    }
    cat('~~~\n')
    cat('  data since', format(attr(x, 'data_since')), '\n')
    cat('  data until', data_till, '\n')
    cat('  wgs84 lon:', paste(lon, collapse = ' .. '), '\n')
    cat('  wgs84 lat:', paste(lat, collapse = ' .. '), '\n')
    cat('  station abbr.:', stations, '\n')
    cat('  param. groups:', groups, '\n')
    cat('  granularities:', unique(x[['parameters']][['parameter_granularity']]), '\n')
    # check search attributes
    sft <- attr(x, 'search_fromto')
    slo <- attr(x, 'search_location')
    spa <- attr(x, 'search_parameters')
    if (any(!is.null(sft), !is.null(slo), !is.null(spa))) {
        cat('  search restrictions:\n')
    }
    if (!is.null(sft)) {
        if (is.finite(sft[1])) {
            cat('   * from:', format(sft[1]), '\n')
        }
        if (is.finite(sft[2])) {
            cat('   * to:', format(sft[2]), '\n')
        }
    }
    if (!is.null(slo)) {
        for (slnm in names(slo)) {
            if (!is.null(slo[[slnm]])) {
                cat('   *', slnm, ':', sapply(slo[[slnm]], paste, 
                    collapse = '..'), '\n')
            }
        }
    }
    if (!is.null(spa)) {
        for (spnm in names(spa)) {
            if (!is.null(spa[[spnm]])) {
                cat('   *', spnm, ':', spa[[spnm]], '\n')
            }
        }
    }
    cat('~~~\n')
    cat('..$datainventory\n')
    #########
    print_dense(x[['datainventory']], ...)
    cat('~~~~~~~~~~~~~~~~~\n')
}

print_dense <- function(x, ntop = 6, nbottom = ntop, center_sep = '---',
    nchars = 10, strict = FALSE) {
    if (is.list(x)) {
        x <- unclass(x)
    }
    x <- as.data.frame(x)
    if (nrow(x) <= ntop + nbottom) {
        xshort <- x
    } else {
        xtop <- head(x, ntop)
        xbot <- tail(x, nbottom)
        xshort <- rbind(
            as.data.frame(lapply(xtop, as.character)),
            rep(center_sep, ncol(x)),
            as.data.frame(lapply(xbot, as.character))
        )
        row.names(xshort)[ntop + 1] <- ' '
        row.names(xshort)[ntop + 1 + seq_len(nbottom)] <- row.names(xbot)
    }
    if (strict) {
        xout <- as.data.frame(lapply(xshort, \(z) {
            pat <- paste0('\\s*(\\S.{', nchars - 1, '}).+')
            sub(pat, '\\1..', z)
        }))
    } else {
        xout <- as.data.frame(lapply(names(xshort), \(z) {
            N <- max(nchars, nchar(z) - 4)
            pat <- paste0('\\s*(\\S.{', N - 1, '}).+')
            sub(pat, '\\1..', xshort[[z]])
        }))
        names(xout) <- names(xshort)
    }
    row.names(xout) <- row.names(xshort)
    print.data.frame(xout, quote = FALSE)
}
# print_dense(metadata[[1]][[2]])
# meta_parameters[[10]]


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
        sc <- collections()
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
# TODO: allow multiple time ranges
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
fuzzy_search <- function(pattern, string, value = FALSE, return_logical = FALSE,
    ignore.case = all(is.na(pmatch(LETTERS, pattern)))) {
    fuz_pat <- paste(c('', unlist(strsplit(pattern, split = '')), ''), collapse = '.*')
    if (return_logical) {
        grepl(fuz_pat, string, ignore.case = ignore.case)
    } else {
        grep(fuz_pat, string, value = value, ignore.case = ignore.case)
    }
}

## re-build meta data ----------------------------------------

if (FALSE) {
    # TODO: make function to update metadata in package data path
    #       -> function to get package path: system.file(package=)
    #       -> name metadata data differently and check if exists in code
    # check collections from MeteoSwiss
    sup <- collections()
    sup

    # get meta data
    # args(get_metadata)
    meta_datainv <- get_metadata(sup, 'data', cache_dir = path_cache)
    meta_stations <- get_metadata(sup, 'stat', cache_dir = path_cache)
    meta_parameters <- get_metadata(sup, 'par', cache_dir = path_cache)

    # rebuild meta data
    metadata <- mapply(\(col, inv, stat, para) {
            col_out <- structure(col$id, title = col$title, 
                description = col$description)
            meta_data <- list(
                assets = structure(col$assets, class = 'ms_assets'),
                datainventory = structure(inv, class = c('ms_datainventory', 'data.frame')),
                stations = structure(stat, class = c('ms_stations', 'data.frame')),
                parameters = structure(para, class = c('ms_parameters', 'data.frame'))
            )
            structure(
                meta_data,
                class = 'ms_metadata',
                # update & add further attributes
                collection = col_out,
                stations = unique(meta_data[['stations']]$station_abbr),
                wgs84_lat = range(meta_data[['stations']]$station_coordinates_wgs84_lat),
                wgs84_lon = range(meta_data[['stations']]$station_coordinates_wgs84_lon),
                parameters = unique(meta_data[['parameters']]$parameter_shortname),
                data_since = min(meta_data[['datainventory']]$data_since),
                data_till = max(meta_data[['datainventory']]$data_till)
            )
        }, 
        attr(sup, 'collections'), meta_datainv, meta_stations, meta_parameters, 
        SIMPLIFY = FALSE
    )
    names(metadata) <- sup
    # save(metadata, file = 'data/metadata.rda')
    save(metadata, file = '~/repos/3_Scripts/8_meteoswiss/data/metadata.rda')
}

