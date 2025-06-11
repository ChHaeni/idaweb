


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
get_datainventory <- function(id) {
    if (length(id) == 1L) {
        # check supported id
        if (i_supp <- check_supported(id)) {
            # get collection info
            id_coll <- attr(i_supp, 'collection')
            browser()
            # get file url
            file_name <- grep('datainventory', names(id_coll$assets), value = TRUE, 
                fixed = TRUE)
            file_url <- id_coll$assets[[file_name]]$href
            last_updated <- id_coll$assets[[file_name]]$updated
            file_checksum <- id_coll$assets[[file_name]][['file:checksum']]
            # datainventory file url
            bu <- 'https://data.geo.admin.ch/'
            read.table(paste0(bu, id, '/', id_stem, 
                '_meta_datainventory.csv'), sep = ';', header = TRUE, fileEncoding = 'Windows-1252',
                fill = TRUE, comment.char = '', quote = '"'
            )
        } else {
            return(invisible(NULL))
        }
    } else {
        # get data
        out <- lapply(id, get_datainventory)
        # remove invalid
        out[!sapply(out, is.null)]
    }
}

get_parameters <- function(id) {
    # get stem
    id_stem <- sub('ch.meteoschweiz.', '', id, fixed = TRUE)
    # base url to REST API
    bu <- 'https://data.geo.admin.ch/'
    read.table(paste0(bu, id, '/', id_stem, 
        '_meta_parameters.csv'), sep = ';', header = TRUE, fileEncoding = 'Windows-1252',
        fill = TRUE, comment.char = '', quote = '"'
    )
}
get_stations <- function(id) {
    # get stem
    id_stem <- sub('ch.meteoschweiz.', '', id, fixed = TRUE)
    # base url to REST API
    bu <- 'https://data.geo.admin.ch/'
    read.table(paste0(bu, id, '/', id_stem, 
        '_meta_stations.csv'), sep = ';', header = TRUE, fileEncoding = 'Windows-1252',
        fill = TRUE, comment.char = '', quote = '"'
    )
}
# get_info <- function(id) {
#     datainventory <- get_datainventory(id)
#     parameters <- get_parameters(id)
#     stations <- get_stations(id)
#     head(datainventory)
#     str(stations)
#     s_cols <- c('station_abr', 'station_name', 'station_canton', 'station_type_en',
#         'station_data_since',
#     head(stations[, 
#     browser()
# }
# get_info(cl[20])

##  • helper functions ====================

# helper function to download data
# and get path to local file
dl_data <- function(url) {
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
        structure(length(i) != 0L, collection = attr(sc, 'collections')[[i]])
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
