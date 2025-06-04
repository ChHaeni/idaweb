
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


# fetch assets of a single collection
get_assets <- function(id = NULL) {
    # check id input
    if (!(length(id) == 1 && is.character(id))) {
        stop('argument "id" is not a valid single collection id!')
    }
    # base url to REST API
    bu <- 'https://data.geo.admin.ch/api/stac/v1/'
    out <- content(GET(paste0(bu, 'collections/', id, '/assets')))
    if (is.null(out$code)) {
        sapply(out$assets, '[[', 'id')
    } else {
        out
    }
}

yy <- get_assets('ch.meteoschweiz.ogd-smn')
lapply(cl, get_assets)

str(yy$assets)

# fetch list of items for a single collection 
get_items <- function(id = NULL) {
    # check id input
    if (!(length(id) == 1 && is.character(id))) {
        stop('argument "id" is not a valid single collection id!')
    }
    # base url to REST API
    bu <- 'https://data.geo.admin.ch/api/stac/v1/'
    out <- content(GET(paste0(bu, 'collections/', id, '/items')))
    if (is.null(out$code)) {
        ids <- sapply(out$features, '[[', 'id')
        nms <- lapply(out$features, \(x) names(x$assets))
        names(nms) <- ids
        nms
    } else {
        out
    }
}

meteoswiss_collections()

xx <- get_items('ch.meteoschweiz.ogd-smn')
get_items(cl[1])

