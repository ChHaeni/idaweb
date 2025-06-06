
library(httr)

# REST API docu
# https://data.geo.admin.ch/api/stac/static/spec/v1/api.html#tag/Data

# fetch all available MeteoSwiss Open Data collections
meteoswiss_collections <- function() {
    # base url to REST API
    bu <- 'https://data.geo.admin.ch/api/stac/v1/'
    out <- content(GET(paste0(bu, 'collections?provider=meteoswiss')))
    if (is.null(out$code)) {
        ids <- sapply(out$collections, '[[', 'id')
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

get_datainventory <- function(id) {
    # get stem
    id_stem <- sub('ch.meteoschweiz.', '', id, fixed = TRUE)
    # base url to REST API
    bu <- 'https://data.geo.admin.ch/'
    read.table(paste0(bu, id, '/', id_stem, 
        '_meta_datainventory.csv'), sep = ';', header = TRUE, fileEncoding = 'Windows-1252',
        fill = TRUE, comment.char = '', quote = '"'
    )
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
