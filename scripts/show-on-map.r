
show_on_map <- function(x) {
    # either stations or metadata or list thereof
    require(leaflet)
    base_map <- addTiles(leaflet())
    # get stations
    sl <- get_station_list(x)
    xbox <- range(lapply(sl, \(s) attr(s, 'search_bbox')[['lon']]))
    ybox <- range(lapply(sl, \(s) attr(s, 'search_bbox')[['lat']]))
    all <- do.call(rbind, sl)
    # get number of stations
    nsl <- sapply(sl, nrow)
    sindex <- unlist(lapply(seq_along(nsl), \(i) rep(i, nsl[i])))
    # get datainv info
    di <- datainventory(x, as_dt = TRUE)
    # get parameter info
    pr <- parameters(x, as_dt = TRUE)
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
        name <- all[i, 'station_name']
        typ <- all[i, 'station_type_en']
        lab <- paste0(name, ' (', stat, ') - ', typ)
        # subset datainventory
        dsub <- di[[sindex[i]]][station_abbr == stat, ]
        # get parameter info
        pshorts <- dsub[, parameter_shortname]
        paras <- pr[[sindex[i]]][parameter_shortname %in% pshorts, .(
            shortname = parameter_shortname,
            group = parameter_group_en,
            granularity = parameter_granularity,
            unit = parameter_unit,
            description = parameter_description_en
            )]
        # table popup
        pop <- paste(
            '<table>
            <caption>',
            lab
            , '</caption>
            <thead>
            <tr>',
            paste(paste0('<th>', names(paras), '</th>'), collapse = '\n')
            , '</tr>
            </thead>
            <tbody>',
            paras[, {
                out <- sapply(.SD, \(x) paste0('<td>', x, '</td>'))
                paste('<tr>', paste(out, collapse = '\n'), '</tr>')
            }, by = .I][, paste(V1, collapse = '\n')]
            , '</tbody>
            </table>', sep = '\n'
        )
        map <- addCircleMarkers(map, lng = lons[i], lat = lats[i], color = cols[[typ]], 
            popup = pop, label = lab)
    }
    # add bbox
    map <- addRectangles(map, lon_range[1], lat_range[1], lon_range[2], lat_range[2],
        fill = FALSE, color = 'darkgrey', opacity = 0.9)
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
        attr(out, 'search_bbox') <- attr(x, 'search_location')
        # append parameters & datainventory
        attr(out, 'parameters') <- parameters(x)
        attr(out, 'datainventory') <- datainventory(x)
    } else {
        as_list <- FALSE
        out <- lapply(x, get_station_list, as_list = as_list)
    }
    if (as_list) {
        out <- list(out)
    }
    out
}
