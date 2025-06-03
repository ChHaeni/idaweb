
library(rstac)

bu <- 'https://data.geo.admin.ch/api/stac/v1/'
# bu <- 'https://data.geo.admin.ch/api/stac/v1/collections/ch.meteoschweiz.ogd-smn'

src <- stac(bu)

get_request(src)

str(src)

cl <- collections(src)

get_request(cl)

mc <- stac_search(
    q = src,
    collections = 'ch.meteoschweiz.ogd-smn'
)

get_request(mc)
