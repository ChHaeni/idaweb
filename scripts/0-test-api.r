
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


