


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
get_metadata <- function(id, type = c('datainventory', 'stations', 'parameters')) {
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
                local_file <- dl_data(file_url, file_checksum)
                return(
                    read.table(local_file, sep = ';', header = TRUE, fill = TRUE,
                        fileEncoding = 'Windows-1252', comment.char = '', quote = '"'
                    )
                )
            }
            # return(idaweb:::metadata[[id]][[file_name]])
            # replace me when package
            return(metadata[[id]][[file_name]])
        } else {
            return(invisible(NULL))
        }
    } else {
        # get data
        out <- lapply(id, get_metadata, type = type)
        # remove invalid
        return(out[!sapply(out, is.null)])
    }
}

# # save metadata
# z1 <- get_metadata(xx, 'data')
# z2 <- get_metadata(xx, 'stat')
# z3 <- get_metadata(xx, 'par')
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

get_tzone <- function(x) {
    out <- attr(x, 'tzone')
    if (is.null(out)) {
        if (inherits(x, 'POSIXct')) {
            out <- ''
        } else {
            out <- 'UTC'
        }
    }
    out
}
search_by_datetime <- function(from, to = NULL, tz = get_tzone(from), previous = NULL) {
    if (is.null(to)) {
        seps <- c('to', '/', '::', ' - ')
        # split any time ranges
        from_list <- strsplit(from, 
            split = paste0(' ?', paste(seps, collapse = ' ?| ?'), ' ?'))
        # parse datetimes to POSIXct
        dt_list <- lapply(from_list, fa_st, tz = tz)
    } else {
        # parse from
        from <- switch(class(from)
            , character = fa_st(from, tz = tz)
            , POSIXlt = as.POSIXct(from)
            , POSIXct = from
            , stop('argument "from" should be of class "character" or "POSIXt"!')
        )
        # parse to
        to <- switch(class(to)
            , character = fa_st(to, tz = tz)
            , POSIXlt = as.POSIXct(to)
            , POSIXct = to
            , stop('argument "to" should be of class "character" or "POSIXt"!')
        )
        # parse datetimes to POSIXct
        dt_list <- mapply('c', from, to, SIMPLIFY = FALSE)
    }
}

fa_st <- function(x, tz) {
    formats <- c("%Y", "%d.%m.%Y", "%d.%m.%y", "%d.%m.%Y %H:%M", "%d.%m.%y %H:%M",
        "%d.%m.%Y %H:%M:%S", "%d.%m.%y %H:%M:%S", "%Y-%m-%d", "%y-%m-%d", 
        "%Y-%m-%d %H:%M", "%y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S", "%y-%m-%d %H:%M:%S")
    lubridate::fast_strptime(x, format = formats, tz = tz, lt = FALSE)
}

search_by_datetime(c('01.01.2018 to 05.02.2018', '13.08.2020', '07.02.2024/08.03.2025'))

search_by_location
search_by_parameter

# add option to provide previous results for further subsetting
# add function to bind different results together
# add function to get data from results

# TODO: add option to provide path to downloaded files

##  • helper functions ====================

# helper function to download data
# and get path to local file
dl_data <- function(url, checksum = NULL) {
    # get data name
    data_name <- basename(url)
    # check if data is already available locally
    local_file <- getOption(data_name)
    if (is.null(local_file)) {
        # temporary file path
        local_file <- tempfile(fileext = data_name)
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
        i <- which(id %in% sc)
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


## testing ----------------------------------------

# test
cl <- meteoswiss_collections()
cl
cl[20]

# get info about parameters and stations for single collections
# -> only ogd data without forecasting, nbcn*, obs or phenology
download_meta_data <- function(id = NULL) {
    # check id input
    if (!(length(id) == 1 && is.character(id))) {
        stop('argument "id" is not a valid single collection id!')
    }
    # download info
    list(
        datainventory = get_datainventory(id),
        parameters = get_parameters(id),
        stations = get_stations(id)
    )
}

get_datainventory(cl[21])
x1 <- lapply(cl[14:23], get_datainventory)
x2 <- lapply(cl[14:23], get_parameters)
x3 <- lapply(cl[14:23], get_stations)



search_meteo <- function(
    ll_wgs84 = c(5.96, 45.82),
    ur_wgs84 = c(10.49, 47.81),
    # ll_lv95 = NULL, ur_lv95 = NULL,
    from = NULL,
    to = NULL,
    ids = NULL,
    collections = NULL
) {

}

# base url to REST API / GET search
valid_collections <- paste(
    # 'ch.meteoschweiz.ogd-nbcn',
    # 'ch.meteoschweiz.ogd-nbcn-precip',
    # 'ch.meteoschweiz.ogd-nime',
    # 'ch.meteoschweiz.ogd-obs',
    # 'ch.meteoschweiz.ogd-phenology',
    # 'ch.meteoschweiz.ogd-pollen',
    'ch.meteoschweiz.ogd-smn',
    'ch.meteoschweiz.ogd-smn-precip',
    'ch.meteoschweiz.ogd-smn-tower',
    'ch.meteoschweiz.ogd-tot',
    sep = ','
)
bu <- paste0('https://data.geo.admin.ch/api/stac/v1/search?collections=', 
    valid_collections, '&')
# t1 <- content(GET(paste0(bu, 'bbox=6.96,45.82,9,46.81')))
bern <- c(46.989090, 7.463082)
ll <- c(46.979143, 7.445185)
ur <- c(46.997198, 7.482103)
bbox <- paste(c(rev(ll), rev(ur)), collapse = ',')
t1 <- content(GET(paste0(bu, 'bbox=', bbox, '&datetime=2024-01-01T00:00:00Z/..')))
# names(t1)
length(t1$features)
# NOTE: always limit of 100!!
names(t1$features[[1]]$assets)

str(t1)

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

xnms <- names(t1$features[[1]]$assets)

xx <- t1$features[[1]]$assets[[xnms[1]]]

tf <- tempfile(fileext = xnms[1])

options(setNames(list(tf), xnms[1]))


if (!file.exists(getOption(xnms[1]))) {
    download.file(url = xx$href, destfile = getOption(xnms[1]))
}

yy <- read.table(getOption(xnms[1]), sep = ';', header = TRUE, fileEncoding = 'Windows-1252')
head(yy)

head(x3[[1]][, 1:20])

# NOTE: searching stations in a single collection might be more efficient 
#   via meta data meta_stations

# -> for package => include meta data as data & check updated
bu <- 'https://data.geo.admin.ch/api/stac/v1/'
out <- content(GET(paste0(bu, 'collections/ch.meteoschweiz.ogd-smn')))
out$assets
sapply(out$assets, '[[', 'updated')

# -> function update_meta_data
