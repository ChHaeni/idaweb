
library(httr)

# REST API docu
# https://data.geo.admin.ch/api/stac/static/spec/v1/api.html#tag/Data

# fetch all available MeteoSwiss Open Data collections
meteoswiss_collections <- function() {
    # base url to REST API
    bu <- 'https://data.geo.admin.ch/api/stac/v1/'
    out <- content(GET(paste0(bu, 'collections?provider=meteoswiss')))
    if (is.null(out$code)) {
        structure(out, class = c('ms_collections', class(out)))
    } else {
        out
    }
}

# print method for collections
print.ms_collections <- function(x, ...) {
    # get ids & titles
    ids <- sapply(x$collections, '[[', 'id')
    titles <- sapply(x$collections, '[[', 'title')
    mt <- max(nchar(titles)) + grepl('[^a-zA-Z0-9:)( -]', titles) * 2
    cat('~~~~~~\n')
    cat('Open Data - MeteoSwiss\n')
    cat('----------------------\n')
    cat('available collections:\n')
    for (i in seq_along(ids)) {
        cat(sprintf(
                paste0('%2i: %-', mt[i], 's -> %s\n')
                , i, titles[i], ids[i]))
    }
    cat('~~~~~~\n')
    invisible()
}

# test
cl <- meteoswiss_collections()
cl

