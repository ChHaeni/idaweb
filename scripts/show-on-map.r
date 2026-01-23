
show_on_map <- function(x) {
    # either stations or metadata or list thereof
    require(leaflet)
    base_map <- addTiles(leaflet())
    # x <- sc1
    # x <- sc3
    # get stations
    sl <- get_station_list(x)
    xbox <- range(lapply(sl, \(s) attr(s, 'search_bbox')[['x']]))
    ybox <- range(lapply(sl, \(s) attr(s, 'search_bbox')[['y']]))
    all <- do.call(rbind, sl)
    # define colors
    types <- unique(all[['station_type_en']])
    cols <- setNames(palette.colors(length(types)), types)
    # get coord range
    lon_range <- range(
        lons <- all[, 'station_coordinates_wgs84_lon'],
        xbox,
        na.rm = TRUE
    )
    lat_range <- range(
        lats <- all[, 'station_coordinates_wgs84_lat'],
        ybox,
        na.rm = TRUE
    )
    # set view point
    map <- fitBounds(base_map, lon_range[1], lat_range[1], lon_range[2], lat_range[2])
    # add stations
    for (i in seq_along(all[[1]])) {
        stat <- all[i, 'station_abbr']
        typ <- all[i, 'station_type_en']
        map <- addCircleMarkers(map, lng = lons[i], lat = lats[i], color = cols[[typ]], 
            label = stat)
    }
    # show map
    map
}
get_station_list <- function(x, as_list = TRUE) {
    if (inherits(x, 'met_stations')) {
        # pass
        out <- x
    } else if (inherits(x, 'met_metadata')) {
        # get stations
        out <- stations(x)
        names(attributes(sc2[[1]]))
        attr(out, 'search_bbox') <- attr(x, 'search_location')
    } else {
        as_list <- FALSE
        out <- lapply(x, get_station_list, as_list = as_list)
    }
    if (as_list) {
        out <- list(out)
    }
    out
}
