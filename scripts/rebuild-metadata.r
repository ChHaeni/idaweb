
## re-build meta data ----------------------------------------

if (FALSE) {
    # TODO: make function to update metadata in package data path
    #       -> function to get package path: system.file(package=)
    #       -> name metadata data differently and check if exists in code
    # check collections from MeteoSwiss
    sup <- collections()
    sup

    # get meta data
    load('data/metadata.rda')
    path_cache <- 'cached'
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

