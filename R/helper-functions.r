
## various helper functions ----------------------------------------

# add helper function to construct url
met_url <- function(...) {
    paste0('https://data.geo.admin.ch/', ...)
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



## search-data ----------------------------------------

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

fix_meta_arg <- function(meta) {
    meta_arg <- deparse(substitute(meta))
    if (missing(meta)) {
        return(idaweb::metadata)
    } else if (is.character(meta)) {
        # change once package
        ind <- sub('ch.meteoschweiz.ogd-', '', names(idaweb::metadata)) %in%
            sub('ch.meteoschweiz.ogd-', '', meta)
        if (any(ind)) {
            # collection name(s)
            if (length(meta) == 1L) {
                meta <- idaweb::metadata[[which(ind)]]
            } else {
                meta <- idaweb::metadata[ind]
            }
        } else {
            meta <- switch(meta
                , 'all' = idaweb::metadata
                # any others?
                # error unknown
                , stop('cannot interpret ',meta_arg ,' argument!', call. = FALSE)
            )
        }
        return(meta)
    } else if (inherits(meta, 'met_metadata')) {
        # meta data of single collection
        return(meta)
    } else if (is.list(meta)) {
        # check list of meta data
        check_list <- unlist(lapply(meta, inherits, 'met_metadata'))
        if (all(check_list)) {
            # list of collections
            return(meta)
        }
    }
    # throw error (TODO: add parent.call via argument)
    stop('cannot interpret ', meta_arg , ' argument!', call. = FALSE)
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

## data-download ----------------------------------------


##  • read downloaded files ====================

.get_data <- function(x, as_DT = TRUE) {
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

##  • get file info ====================

.get_files <- function(x, cache_dir = tempdir(), force_cache = FALSE) {
    # check if more than one collection
    # also check class
    # get collection
    cl <- x$collection
    # loope over file list
    structure(c(
        list(collection = cl),
        lapply(x[-1], \(l) {
            # get info on station
            info <- httr::content(httr::GET(met_url('api/stac/v1/collections/', cl, '/items/', 
                        l$station)))$assets
            # download files
            c(
                l,
                files = {
                    out <- lapply(l$file_list, \(fl) {
                    # what if missing?
                    if (fl$filename %in% names(info)) {
                        .dl_data(met_url(cl, '/', l$station, '/', fl$filename), 
                            checksum = info[[fl$filename]][['file:checksum']], 
                            cache_dir = cache_dir, force_cache = force_cache)
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

##  • get file names ====================

# add function to get data
.get_filenames <- function(meta_data) {
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
            .file_names(x, from, to, now, pre, yesterday_12UTC, current_year_jan1)
        })
    ), class = 'file_list')
}

##  • create relevant files names ====================

.file_names <- function(x, from, to, now, pre, yd12, cy_jan) {
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

##  • download files ====================

# helper function to download data
# and get path to local file
.dl_data <- function(url, checksum = NULL, cache_dir = tempdir(), 
    force_cache = FALSE) {
    # get data name
    data_name <- basename(url)
    # check if data is already available locally
    if (force_cache) {
        local_file <- NULL
    } else {
        local_file <- getOption(data_name)
    }
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



