
# print method for collections
print.met_collections <- function(x, ...) {
    # get titles
    titles <- sapply(attr(x, 'collections'), '[[', 'title')
    mt <- max(nchar(titles)) + grepl('[^a-zA-Z0-9:)( -]', titles) * 2
    cat('~~~~~~~~~~~~~~~~~~~~~~\n')
    cat('MeteoSwiss Open Data (Ground-based measurements)\n')
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
    groups <- paste(ug <- unique(x[['parameters']][['parameter_group_en']]), collapse = ';')
    if (nchar(groups) > 40) {
        groups <- sub('^(.{10,20}[;]).+(;.{10,20})$', '\\1...\\2', groups)
    }
    # shorten stations
    stations <- paste(us <- unique(x[['stations']][['station_name']]), collapse = ';')
    if (nchar(stations) > 40) {
        stations <- sub('^(.{10,20}[;]).+(;.{10,20})$', '\\1...\\2', stations)
    }
    # fix till
    data_till <- attr(x, 'data_till')
    if (is.na(data_till) && lubridate::is.POSIXct(data_till)) {
        data_till <- 'today'
    } else {
        data_till <- format(data_till)
    }
    # fix lon 
    # show only 3 digits (1e-3° is approx 100 m)
    if (any(is.na(attr(x, 'wgs84_lon')))) {
        lon <- lat <- NA_character_
    } else {
        lon <- sub('^0', ' ', sprintf('%06.3f', unique(attr(x, 'wgs84_lon'))))
        lat <- sprintf('%06.3f', unique(attr(x, 'wgs84_lat')))
        if (length(lon) == 2) {
            lon <- paste(lon, collapse = ' .. ')
        }
        if (length(lat) == 2) {
            lat <- paste(lat, collapse = ' .. ')
        }
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
    cat('  wgs84 lon:', lon, '\n')
    cat('  wgs84 lat:', lat, '\n')
    # cat('  station abbr.:', stations, '\n')
    if (length(us) == 1) {
        cat('  station name:', stations, '\n')
    } else {
        cat('  station names:', stations, '\n')
    }
    if (length(ug) == 1) {
        cat('  param. group:', groups, '\n')
    } else {
        cat('  param. groups:', groups, '\n')
    }
    grans <- unique(x[['parameters']][['parameter_granularity']])
    if (length(grans) == 1) {
        cat('  granularity:', grans, '\n')
    } else {
        cat('  granularities:', grans, '\n')
    }
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
                if (slnm %in% c('x', 'y')) {
                    # FIXME: fix lists of length > 1
                    sloxy <- slo[[slnm]][[1]]
                    sloxy <- sub('^0', ' ', sprintf('%06.3f', unique(sloxy)))
                    if (length(sloxy) == 2) {
                        sloxy <- paste(sloxy, collapse = ' .. ')
                    }
                    cat('   *', slnm, ':', sloxy, '\n')
                } else {
                    cat('   *', slnm, ':', sapply(slo[[slnm]], paste, 
                        collapse = '..'), '\n')
                }
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

