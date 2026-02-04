
## fuzzy search (exported) ----------------------------------------

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





## search-data ----------------------------------------

check_xy_arg <- function(xy) {
    if (missing(xy) || is.null(xy)) {
        return(NULL)
    }
    if (is.list(xy) && all(sapply(xy, is.numeric)) &&
        unique(lengths(xy)) == 2L) {
        # ok to test
        xyl <- xy
    } else if (is.numeric(xy)) {
        if (length(xy) != 2) {
            xy_nm <- deparse(substitute(xy))
            stop('if argument', xy_nm, 'is numeric, its length must be 2')
        }
        xyl <- list(xy)
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
    }
    # fix coord values
    v_out <- lapply(xyl, \(z) {
        v <- as.numeric(z)
        # check order
        if (v[2] < v[1]) {
            stop('Coordinate range inverted?')
        }
        # lon 4 to 12
        if (all(v >= 4 & v <= 12 | is.infinite(v))) {
            # longitude ok
            return(v)
        } else if (all(v >= 42 & v <= 50 | is.infinite(v))) {
            # latitude ok
            return(v)
        }
        # check swiss coords
        if (all(v >= 60 & v <= 310)) {
            # 60 - 310 -> y lv03
            v <- v * 1e3
        } else if (all(v >= 460 & v <= 860)) {
            # 460 - 860 -> x lv03
            v <- v * 1e3
        } else if (all(v >= 1060 & v <= 1310)) {
            # 1060 - 1310 -> y lv95
            v <- v * 1e3
        } else if (all(v >= 2460 & v <= 2860)) {
            # 2460 - 2860 -> x lv95
            v <- v * 1e3
        } 
        # needs conversion to WGS84
        if (all(v >= 60000 & v <= 310000) || all(v >= 460000 & v <= 860000)) {
            # lv03 ok
        } else if (all(v >= 1060000 & v <= 1310000) || all(v >= 2460000 & v <= 2860000)) {
            # lv95 ok
        } else {
            stop('provided coordinates are not in a valid range')
        }
        # capture non-Inf at end & throw error
        v
    })
    # return list of values
    v_out
}

# transform ch1903 to wgs84
fix_wgs84 <- function(x, y) {
    if (!requireNamespace('sf', quietly = TRUE)) {
        stop('package "sf" is required to transform CH-coordinates - run install.packages("sf") to install')
    }
    # parse x
    xv <- check_xy_arg(x)
    # parse y
    yv <- check_xy_arg(y)
    # fix wgs84
    mapply(\(x, y) {
        if (all(y < 50, x < 12)) {
            # wgs84 ok
            return(list(x = x, y = y))
        } else if (all(x < 1e6, y < 2e6)) {
            # lv03
            crs_from <- 'EPSG:21781'
        } else if (all(x > 1e6, y > 2e6)) {
            # lv95
            crs_from <- 'EPSG:2056'
        } else {
            stop('Mixed x/y coordinate system cannot be handled')
        }
        # transform
        out <- sf::sf_project(crs_from, 'EPSG:4326', cbind(x, y))
        # return list of x/y
        list(x = out[, 1], y = out[, 2])
    }, xv, yv, SIMPLIFY = FALSE)
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

# https://opendatadocs.meteoswiss.ch/general/download#data-granularity
# all times in UTC
# t: The sum, mean or max/min of the last 10 minutes (ReferenceTS 16:00 = 15:50:01 to 16:00:00)
# h: The sum, mean or max/min of the last six 10min-values (ReferenceTS 16:00 = 15:10 to 16:00). Please note: Hourly values before 2018 were calculated differently based on the SYNOP schedule (ReferenceTS 16:00 = 15:50 to 16:40)!
# d: For most parameters the sum, mean or max/min from 00:00 to 23:50 of the according date. Exception for precipitation and snow (manual measurement times used for consistency) where the interval is 6:00 UTC until 5:50 UTC tomorrow (ReferenceTS 22.6.2023 = 22.6.2023 6:10 UTC to 23.6.2023 6:00 UTC)
# m: The sum, mean or max/min of the whole month from 1st to last day of month (ReferenceTS 1.6.2023 = 1.6.2023 00:10 UTC to 30.6.2023 24:00 UTC)
# y: The sum, mean or max/min of the whole year (ReferenceTS 1.1.2023 = 1.1.2023 00:10 UTC to 31.12.2023 24:00 UTC)

.get_data <- function(x, single_timestamp = TRUE, 
    outclass = c('data.table', 'data.frame', 'ibts', 'df', 'dt'),
    outstruc = c('split-all', 'by-station', 'by-granularity', 'cbind-all')
    ) {
    # check conversion
    if (outclass[1] == 'ibts') {
        if (!requireNamespace('ibts', quietly = TRUE)) {
            stop('package ibts is missing - install package from https://github.com/ChHaeni/ibts')
        }
        if (outstruc[1] != 'split-all') {
            warning('conversion to ibts implies "split-all" output structure (ignoring argument)')
        }
        outstruc <- 'split-all'
        if (single_timestamp) {
            warning('conversion to ibts requires single_timestamp = FALSE (ignoring argument)')
        }
        single_timestamp <- FALSE
    }
    # get metadata
    pars <- parameters(metadata[[x[[1]]]])
    stats <- stations(metadata[[x[[1]]]])
    inv <- datainventory(metadata[[x[[1]]]])
    # loop over splits
    out <- lapply(x[-1], \(sp) {
        # time format
        time_format <- '%d.%m.%Y %H:%M'
        # time_format <- switch(sp$granularity
        #     , 'h' = 
        #     , 't' =
        #     , 'd' = 
        #     , 'm' = 
        #     , 'y' = '%d.%m.%Y %H:%M'
        # )
        # loop over files
        d_list <- lapply(sp$file_list, \(fl) {
            suppressWarnings(
                dat <- fread(sp$files[[fl$filename]], select = c('station_abbr', 
                        'reference_timestamp', sp$parameters))
            )
            # add granularity
            dat[, granularity := toupper(sp$granularity)]
            # parse times & subset
            dat[, time := lubridate::fast_strptime(reference_timestamp, 
                format = time_format, lt = FALSE)][, reference_timestamp := NULL]
            # fix times
            switch(sp$granularity
                , 't' = {
                    # add st/et
                    dat[, st := time - 10 * 60]
                    dat[, et := time]
                }
                , 'h' = {
                    if (year(fl$from) < 2018) cat('Fix SYNOP time range before 2018...\n')
                    # fix hourly data before 2018
                    h_shift <- 40 * 60
                    dat[time < lubridate::fast_strptime('01.01.2018', format = '%d.%m.%Y',
                        lt = FALSE, tz = 'UTC'), time := time + h_shift]
                    # add st/et
                    dat[, st := time - 60 * 60]
                    dat[, et := time]
                    # fix overlapping 2017->2018
                    if (year(fl$from) < 2018 && year(fl$to) > 2017) {
                        ind <- dat[, which(as.numeric(diff(st), units = 'secs') < 3600)]
                        if (length(ind) > 0) {
                            dat[ind + 0:1, st := {
                                c(st[1], et[1])
                            }]
                        }
                    }
                }
                , 'd' = {
                    if (grepl('ogd-nime_', fl$filename, fixed = TRUE)) {
                        # fix manual precipitation measurements
                        p_shift <- 6 * 3600
                        dat[, time := time + p_shift]
                    }
                    # add st/et
                    dat[, st := time]
                    dat[, et := time + 24 * 3600]
                }
                , 'm' = {
                    # add st/et
                    dat[, st := time]
                    dat[, et := {
                        out <- st
                        mout <- month(st) + 1
                        yadd <- mout > 12
                        mout[yadd] <- 1
                        month(out) <- mout
                        year(out[yadd]) <- year(out[yadd]) + 1
                        out
                    }]
                }
                , 'y' = {
                    # add st/et
                    dat[, st := time]
                    dat[, et := {
                        out <- st
                        year(out) <- year(st) + 1
                        out
                    }]
                }
            )
            # subset date/time
            dat[et > fl$from & st < fl$to]
        }) # end of inner lapply
        # bind together
        dout <- rbindlist(d_list, fill = TRUE)
        # order by et
        dout <- dout[order(et)]
        # check timestamp
        if (single_timestamp) {
            # remove st/et
            dout[, c('st', 'et') := NULL]
            # sort by time as first column
            setcolorder(dout, c('time', 'station_abbr', 'granularity'))
            setorder(dout, 'time')
        } else {
            # remove time
            dout[, time := NULL]
            # sort by st/et as first column
            setcolorder(dout, c('st', 'et', 'station_abbr', 'granularity'))
            setorder(dout, 'et')
        }
        # return with correct class
        if (outclass[1] %in% c('data.frame', 'df')) {
            dout <- as.data.frame(dout)
        } else if (outclass[1] == 'ibts') {
            dout <- ibts::as.ibts(dout)
        } else {
            setattr(dout, 'parameters', pars[pars$parameter_shortname %in% sp$parameters,])
            setattr(dout, 'stations', stats[stats$station_abbr == toupper(sp$station),])
            return(dout[])
        }
        attr(dout, 'parameters') <- pars[pars$parameter_shortname %in% sp$parameters,]
        attr(dout, 'stations') <- stats[stats$station_abbr == toupper(sp$station),]
        dout
    }) # end of outer lapply
    # fix output structure
    switch(outstruc[1]
        # list output gathered by station
        , 'by-station' = {
            # get stations
            stat <- sub('/.$', '', names(out))
            out <- sapply(unique(stat), \(l) {
                ind <- which(stat == l)
                # gather parameters
                p <- unique(do.call(rbind, lapply(out[ind], attr, 'parameters')))
                # get station
                s <- attr(out[[ind[1]]], 'stations')
                # bind together
                if (outclass[1] %in% c('data.table', 'dt')) {
                    o <- rbindlist(out[ind], fill = TRUE)
                    setattr(o, 'parameters', p)
                    setattr(o, 'stations', s)
                } else {
                    o <- rbind_list(out[ind])
                    attr(o, 'parameters') <- p
                    attr(o, 'stations') <- s
                }
                # return
                o
            }, simplify = FALSE)
        }
        # list output gathered by granularity
        , 'by-granularity' = {
            # get granularities
            gran <- sub('^[^/]+/', '', names(out))
            out <- sapply(unique(gran), \(l) {
                ind <- which(gran == l)
                # gather stations
                s <- unique(do.call(rbind, lapply(out[ind], attr, 'stations')))
                # gather parameters
                p <- unique(do.call(rbind, lapply(out[ind], attr, 'parameters')))
                # bind together
                if (outclass[1] %in% c('data.table', 'dt')) {
                    o <- rbindlist(out[ind], fill = TRUE)
                    setattr(o, 'parameters', p)
                    setattr(o, 'stations', s)
                } else {
                    o <- rbind_list(out[ind])
                    attr(o, 'parameters') <- p
                    attr(o, 'stations') <- s
                }
                # return
                o
            }, simplify = FALSE)
        }
        # all output as a single data.frame/data.table
        , 'cbind-all' = {
            # gather stations
            s <- unique(do.call(rbind, lapply(out, attr, 'stations')))
            # gather parameters
            p <- unique(do.call(rbind, lapply(out, attr, 'parameters')))
            # bind together
            if (outclass[1] %in% c('data.table', 'dt')) {
                out <- rbindlist(out, fill = TRUE)
                setattr(out, 'parameters', p)
                setattr(out, 'stations', s)
            } else {
                out <- rbind_list(out)
                attr(out, 'parameters') <- p
                attr(out, 'stations') <- s
            }
        }
    )
    # return
    out
}

# rbind a list with fill=TRUE
rbind_list <- \(x_list, deparse.level = 1) {
    if (length(x_list) == 1) {
        return(x_list[[1]])
    }
    # remove names
    names(x_list) <- NULL
    # get names
    cls_list <- lapply(x_list, \(z) setNames(sapply(z, \(x) class(x)[1]), names(z)))
    cls_vec <- unlist(cls_list)
    nms <- unique(names(cls_vec))
    # extend
    for (i in seq_along(x_list)) {
        # get rows
        nr <- nrow(x_list[[i]])
        # get missing columns
        mc <- setdiff(nms, names(cls_list[[i]]))
        if (length(mc) > 0) {
            # dummy df
            dummy <- as.data.frame(sapply(mc, \(m) {
                    out <- rep(NA, nr)
                    mode(out) <- cls_vec[m]
                    out
                }, simplify = FALSE))
            # extend
            x_list[[i]] <- cbind(x_list[[i]], dummy)[, nms]
        }
    }
    # bind together
    do.call(rbind, c(x_list, list(deparse.level = deparse.level)))
}

##  • get file info ====================

.get_files <- function(x, cache_dir = NULL, force_cache = FALSE) {
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
                            cache_dir, force_cache = force_cache,
                            checksum = info[[fl$filename]][['file:checksum']]) 
                    } else {
                        browser()
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
    # wrong: historical    (meas. start until 31.12 last year): once a year        (m, d, h, t)
    # wrong: recent        (1.1. current year until yesterday): daily at 12UTC     (m, d, h, t)
    # historical    (meas. start until 31.12 last year): once a year        (d, h, t)
    # recent        (1.1. current year until yesterday): daily at 12UTC     (d, h, t)
    # now           (yesterday 12UTC to now):            every 10 min       (h, t)
    # no type                                            varies             (m, y)
    file_list <- list()
    # station
    stat <- tolower(x[['station_abbr']][1])
    # granularity
    gran <- tolower(x[['parameter_granularity']][1])
    if (gran %in% c('m', 'y')) {
        # -> check file names! => do they really always look the same?
        file_list <- c(file_list, list(list(
                filename = paste0(pre, '_', stat, '_', gran, '.csv'),
                from = from,
                to = min(to, now)
            )))
    } else {
        if (is.na(to)) {
            to <- now
        }
        # add yesterday cut
        if (gran %in% c('t', 'h') && from <= now && to > yd12) {
            file_list <- c(file_list, list(list(
                    filename = paste(pre, stat, gran, 'now.csv', sep = '_'),
                    from = max(from, yd12),
                    to = min(to, now)
                )))
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
            if (gran %in% c('t', 'h')) {
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
            } else {
                file_list <- c(file_list,
                    list(list(
                        filename = paste(pre, stat, gran, 'historical.csv', sep = '_'),
                        from = from,
                        to = min(to, cy_jan)
                    ))
                )
            }
        }
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
.dl_data <- function(url, cache_dir = NULL, force_cache = FALSE, checksum = NULL) {
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
        # check cache_dir
        if (is.null(cache_dir)) cache_dir <- tempdir()
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
    } else if (!is.null(cache_dir) && !grepl(cache_dir, local_file, fixed = TRUE)) {
        warning('file: "', data_name, '" has already been downloaded to "', 
            dirname(local_file), '".\n -> ignoring cache_dir argument.')
    }
    cat('data available at', local_file, '\n')
    # return path
    invisible(local_file)
}



