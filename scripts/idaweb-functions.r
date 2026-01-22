


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
# - add option to convert to ibts
# - fetch data info
# - meta data included in package -> check if update needed
# - function to update specific or all meta data (if necessary)
# - download data only if not available in options
# - one function to get data (incl. options check)
# - show_on_map() => visualize subset on map
# - convenience functions:
#       * fix info()

## functions ----------------------------------------

##  • header ====================

# add helper function to construct url
met_url <- function(...) {
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
        get_url <- met_url('api/stac/v1/collections/ch.meteoschweiz.ogd-', x)
        # get info
        httr::content(httr::GET(get_url))
    })
    # get ids
    ids <- sapply(out, '[[', 'id')
    attr(ids, 'collections') <- out
    structure(ids, class = 'met_collections')
}

# # function to get info on specific data set(s) from collection
# # e.g.: info(collections())
# # x -> either met_collections object or a valid id
# info <- function(x, i = NULL) {
# }

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
            meta_last_updated <- lubridate::fast_strptime(
                idaweb::metadata[[id]]$assets[[file_name]]$updated,
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
                    warning(file_name, ' has recent changes:',
                        'metadata.rda in package needs updating!')
                    # ?
                }
                # download file
                local_file <- .dl_data(file_url, file_checksum, cache_dir = cache_dir)
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
            return(idaweb::metadata[[id]][[nm]])
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
    if (inherits(meta_data, 'met_metadata')) {
        out <- meta_data$parameters
        if (!is.null(cols)) {
            out <- out[, cols]
            if (uniq) {
                out <- unique(out)
            }
        }
    } else if (is.list(meta_data)) {
        out <- sapply(meta_data, parameters, cols = cols, uniq = uniq, 
            simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
    out
}
stations <- function(meta_data) {
    if (inherits(meta_data, 'met_metadata')) {
        meta_data$stations
    } else if (is.list(meta_data)) {
        sapply(meta_data, stations, simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
}
datainventory <- function(meta_data) {
    if (inherits(meta_data, 'met_metadata')) {
        meta_data$datainventory
    } else if (is.list(meta_data)) {
        sapply(meta_data, datainventory, simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
}

# add option to provide previous results for further subsetting
# add function to bind different results together
# add function to get data from results


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

get_data <- function(meta_data, cache_dir = tempdir(), as_DT = TRUE, 
    force_cache = FALSE) {
    if (inherits(meta_data, 'met_metadata')) {
        # get filenames etc.
        meta_data <- .get_filenames(meta_data)
    }
    if (inherits(meta_data, 'file_list')) {
        # get files
        meta_data <- .get_files(meta_data, cache_dir = cache_dir, 
            force_cache = force_cache)
    }
    if (inherits(meta_data, 'dl_files')) {
        # get data from files
        .get_data(meta_data, as_DT = as_DT)
    } else {
        # loop over list
        sapply(meta_data, get_data, cache_dir = cache_dir, as_DT = as_DT,
            force_cache = force_cache, simplify = FALSE)
    }
}

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


## methods ----------------------------------------

# print method for collections
print.met_collections <- function(x, ...) {
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
print.met_assets <- function(x, ...) {
    ncs <- nchar(nms <- names(x))
    names(ncs) <- nms
    cat('-- assets --\n')
    for (nm in nms) {
        sadd <- paste(rep(' ', max(ncs) - ncs[nm] + 1), collapse = '')
        cat(nm, sadd, 'last updated:', x[[nm]][['updated']], '\n')
    }
    invisible()
}

print.met_datainventory <- function(x, ...) {
    cat('-- datainventory --\n')
    cat('Number of data:', length(x[[1]]), '\n')
    cat('Number of stations: ', length(unique(x[[1]])), '\n')
    cat('Number of parameters:', length(unique(x[[2]])), '\n')
    print_dense(x, ...)
}

print.met_stations <- function(x, ...) {
    cat('-- stations --\n')
    cat('Number of stations:', length(x[[1]]), '\n')
    print_dense(x, ...)
}

print.met_parameters <- function(x, ...) {
    cat('-- parameters --\n')
    cat('Number of parameters:', length(x[[1]]), '\n')
    cat('Granularities:', unique(x[['parameter_granularity']]), '\n')
    print_dense(x, ...)
}

print.met_metadata <- function(x, ...) {
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


##  • helper functions ====================

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
    load('~/repos/3_Scripts/8_meteoswiss/data/metadata.rda')
    path_cache <- '~/repos/3_Scripts/8_meteoswiss/cached'
    # args(get_metadata)
    meta_datainv <- get_metadata(sup, 'data', cache_dir = path_cache)
    meta_stations <- get_metadata(sup, 'stat', cache_dir = path_cache)
    meta_parameters <- get_metadata(sup, 'par', cache_dir = path_cache)

    # rebuild meta data
    meta_data <- mapply(\(col, inv, stat, para) {
            col_out <- structure(col$id, title = col$title, 
                description = col$description)
            meta_data <- list(
                assets = structure(col$assets, class = 'met_assets'),
                datainventory = structure(inv, class = c('met_datainventory', 'data.frame')),
                stations = structure(stat, class = c('met_stations', 'data.frame')),
                parameters = structure(para, class = c('met_parameters', 'data.frame'))
            )
            structure(
                meta_data,
                class = 'met_metadata',
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
    names(meta_data) <- sup
    save(meta_data, file = 'data/metadata.rda')

}

