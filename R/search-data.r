
## search wrapper (main function) ----------------------------------------

# search wrapper
met_search <- function(
    # by datetime
    from, to, tz = get_tzone(from, to), 
    # by location
    x, y, z, abbr, name, canton,
    # by parameter
    shortname, unit, group, description, 
    granularity = c('T', 'H', 'D', 'M', 'Y'), 
    language = c('en', 'de', 'fr', 'it'), 
    # all
    meta_data = idaweb::metadata, drop_nodata = TRUE
) {
    # first search by location
    if (!all(missing(x), missing(y), missing(z), missing(abbr),
            missing(name), missing(canton))) {
        meta_data <- search_by_location(x = x, y = y, z = z, abbr = abbr, 
            name = name, canton = canton, meta_data = meta_data, 
            drop_nodata = FALSE)
    }
    # second serach by parameter
    if (!all(missing(shortname), missing(unit), missing(group), 
            missing(description), missing(granularity))) {
        meta_data <- search_by_parameter(shortname = shortname, unit = unit, 
            group = group, description = description, language = language, 
            granularity = granularity, meta_data = meta_data, 
            drop_nodata = FALSE)
    }
    # third search by date/time
    if (!all(missing(from), missing(to))) {
        meta_data <- search_by_datetime(from = from, to = to, tz = tz, 
            meta_data = meta_data, drop_nodata = FALSE)
    }
    # drop empty?
    if (drop_nodata) {
        meta_data <- meta_data[sapply(meta_data, \(x) nrow(x$datainventory) > 0)]
    }
    meta_data
}

## individual search functions ----------------------------------------

##  • by date & time ====================

search_by_datetime <- function(from, to, tz = get_tzone(from, to), 
    meta_data = idaweb::metadata, drop_nodata = FALSE) {
    # fix meta argument
    meta_data <- fix_meta_arg(meta_data)
    # parse from & to
    fromto <- check_fromto(from, to, tz = tz)
    # select datainventory/station/parameters
    if ('datainventory' %in% names(meta_data)) {
        if (!is.null(sft <- attr(meta_data, 'search_fromto'))) {
            # check from
            fromto[1] <- max(sft[1], fromto[1])
            # check to
            fromto[2] <- min(sft[2], fromto[2])
        }
        # TODO: improve these if/else tests! and capture errors
        # check from
        i_from <- is.na(meta_data$datainventory$data_till) | 
            fromto[1] <= meta_data$datainventory$data_till
        # check to
        i_to <- i_from & meta_data$datainventory$data_since <= fromto[2]
        # return subset incl from/to
        sub_inv <- meta_data$datainventory[i_to, ]
        if (drop_nodata && nrow(sub_inv) == 0) {
            return(NULL)
        }
        # get stations
        sub_stats <- meta_data$stations[meta_data$stations$station_abbr %in% 
            sub_inv$station_abbr, ]
        # get parameters
        sub_paras <- meta_data$parameters[meta_data$parameters$parameter_shortname %in% 
            sub_inv$parameter_shortname, ]
        if (nrow(sub_inv) > 0) {
            data_since <- min(sub_inv$data_since)
            data_till <- max(sub_inv$data_till)
            wgs84_lat <- range(sub_stats$station_coordinates_wgs84_lat)
            wgs84_lon <- range(sub_stats$station_coordinates_wgs84_lon)
            parameters <- unique(sub_paras$parameter_shortname)
            stations <- unique(sub_stats$station_abbr)
        } else {
            data_since <- lubridate::NA_POSIXct_
            data_till <- NA_integer_
            wgs84_lat <- c(NA_real_, NA_real_)
            wgs84_lon <- c(NA_real_, NA_real_)
            parameters <- NA_character_
            stations <- NA_character_
        }
        structure(
            list(
                assets = meta_data$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'met_metadata', 
            # pass collection
            collection = attr(meta_data, 'collection'),
            # update further attributes
            stations = stations,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            data_since = data_since,
            data_till = data_till,
            search_fromto = fromto,
            search_location = attr(meta_data, 'search_location'),
            search_parameters = attr(meta_data, 'search_parameters')
        )
    } else {
        out <- sapply(meta_data, search_by_datetime, from = fromto[1], 
            to = fromto[2], tz = tz, drop_nodata = drop_nodata,
            simplify = FALSE)
        if (drop_nodata) {
            out[!sapply(out, is.null)]
        } else {
            out
        }
    }
}

##  • by location ====================

search_by_location <- function(x, y, z, abbr, name, canton, 
    meta_data = idaweb::metadata, drop_nodata = FALSE) {
    # valid search entries:
    # lat & lon: '46.1..46.2', '46.1 to 46.2', '46.1/46.2', c(46.1, 46.2), 
    # TODO: ch_x & ch_y: same as above BUT additionally, only 100-thousands 
    #   -> distinguish between lv95 and lv03
    #   -> check x/y as required in R and possibly flip
    # only attach sf if really necessary
    # fix meta argument
    meta_data <- fix_meta_arg(meta_data)
    # parse x
    xv <- check_xy_arg(x)
    # parse y
    yv <- check_xy_arg(y)
    # parse z
    zv <- check_z_arg(z)
    # select datainventory/station/parameters
    if ('datainventory' %in% names(meta_data)) {
        if (!is.null(sft <- attr(meta_data, 'search_location'))) {
            stop('Fix already searched by location')
        }
        search_location <- list()
        # get number of stations
        n_stations <- nrow(meta_data$stations)
        # TODO: improve these if/else tests! and capture errors
        # check x/lon
        if (is.null(xv)) {
            i_x <- rep(TRUE, n_stations)
        } else {
            # subset by longitude
            s_lon <- meta_data$stations$station_coordinates_wgs84_lon 
            i_x <- unlist(lapply(xv, \(v) s_lon >= v[1] & s_lon <= v[2]))
            search_location <- c(search_location, list(x = x))
        }
        # check y/lat
        if (is.null(yv)) {
            i_y <- rep(TRUE, n_stations)
        } else {
            # subset by latitude
            s_lat <- meta_data$stations$station_coordinates_wgs84_lat 
            i_y <- unlist(lapply(yv, \(v) s_lat >= v[1] & s_lat <= v[2]))
            search_location <- c(search_location, list(y = y))
        }
        # check z/elevation
        if (is.null(zv)) {
            i_z <- rep(TRUE, n_stations)
        } else {
            # subset by elevation
            s_el <- meta_data$stations$station_height_masl
            i_z <- unlist(lapply(zv, \(v) s_el >= v[1] & s_el <= v[2]))
            search_location <- c(search_location, list(z = z))
        }
        # check abbr
        if (missing(abbr) || is.null(abbr)) {
            i_abbr <- rep(TRUE, n_stations)
        } else {
            i_abbr <- meta_data$stations$station_abbr %in% abbr
            search_location <- c(search_location, list(abbr = abbr))
        }
        # check name
        if (missing(name) || is.null(name)) {
            i_name <- rep(TRUE, n_stations)
        } else {
            i_name <- unlist(lapply(name, fuzzy_search, 
                meta_data$stations$station_name, return_logical = TRUE))
            search_location <- c(search_location, list(name = name))
        }
        # check canton
        if (missing(canton) || is.null(canton)) {
            i_canton <- rep(TRUE, n_stations)
        } else {
            i_canton <- meta_data$stations$station_canton %in% canton
            search_location <- c(search_location, list(canton = canton))
        }
        # combine all
        i_ok <- i_x & i_y & i_z & i_abbr & i_name & i_canton
        # return subset of stations
        sub_stats <- meta_data$stations[i_ok, ]
        # get inventory
        sub_inv <- meta_data$datainventory[meta_data$datainventory$station_abbr 
            %in% sub_stats$station_abbr, ]
        if (drop_nodata && nrow(sub_inv) == 0) {
            return(NULL)
        }
        # get parameters
        sub_paras <- meta_data$parameters[meta_data$parameters$parameter_shortname %in% 
            sub_inv$parameter_shortname, ]
        if (nrow(sub_inv) > 0) {
            data_since <- min(sub_inv$data_since)
            data_till <- max(sub_inv$data_till)
            wgs84_lat <- range(sub_stats$station_coordinates_wgs84_lat)
            wgs84_lon <- range(sub_stats$station_coordinates_wgs84_lon)
            parameters <- unique(sub_paras$parameter_shortname)
            stations <- unique(sub_stats$station_abbr)
        } else {
            data_since <- lubridate::NA_POSIXct_
            data_till <- NA_integer_
            wgs84_lat <- c(NA_real_, NA_real_)
            wgs84_lon <- c(NA_real_, NA_real_)
            parameters <- NA_character_
            stations <- NA_character_
        }
        structure(
            list(
                assets = meta_data$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'met_metadata', 
            # pass collection
            collection = attr(meta_data, 'collection'),
            # update further attributes
            stations = stations,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            data_since = data_since,
            data_till = data_till,
            search_fromto = attr(meta_data, 'search_fromto'),
            search_location = search_location,
            search_parameters = attr(meta_data, 'search_parameters')
        )
    } else {
        out <- sapply(meta_data, search_by_location, x = xv, y = yv, 
            z = zv, abbr = abbr, name = name,
            canton = canton, drop_nodata = drop_nodata, 
            simplify = FALSE
        )
        if (drop_nodata) {
            out[!sapply(out, is.null)]
        } else {
            out
        }
    }
}

##  • by parameter ====================

search_by_parameter <- function(shortname, unit, group, description, 
    granularity = c('T', 'H', 'D', 'M', 'Y'), 
    language = c('en', 'de', 'fr', 'it'), 
    meta_data = idaweb::metadata, drop_nodata = FALSE) {
    # fix meta argument
    meta_data <- fix_meta_arg(meta_data)
    if ('datainventory' %in% names(meta_data)) {
        sub_paras <- meta_data$parameter
        language <- match.arg(language)
        search_parameters <- list()
        if (!missing(shortname)) {
            # search by shortname (vector)
            ind <- unlist(lapply(shortname, grep, sub_paras$parameter_shortname))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(shortname = shortname))
        }
        if (!missing(unit)) {
            # search by unit (vector)
            ind <- unlist(lapply(unit, grep, sub_paras$parameter_unit))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(unit = unit))
        }
        if (!missing(group)) {
            # search by group (vector)
            ind <- unlist(lapply(group, fuzzy_search,
                    sub_paras[[paste0('parameter_group_', language)]]))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(group = group))
        }
        if (!missing(granularity)) {
            # ignore case
            granularity <- toupper(granularity)
            # search by granularity (vector)
            ind <- sub_paras$parameter_granularity %in% granularity
            sub_paras <- sub_paras[ind, ]
            search_parameters <- c(search_parameters, list(granularity = granularity))
        }
        if (!missing(description)) {
            # search by group (vector)
            ind <- unlist(lapply(description, fuzzy_search, 
                sub_paras[[paste0('parameter_description_', language)]]))
            sub_paras <- sub_paras[unique(ind), ]
            search_parameters <- c(search_parameters, list(description = description))
        }
        # get inventory
        sub_inv <- meta_data$datainventory[
            meta_data$datainventory$parameter_shortname %in% 
            sub_paras$parameter_shortname, ]
        if (drop_nodata && nrow(sub_inv) == 0) {
            return(NULL)
        }
        # get stations
        sub_stats <- meta_data$stations[meta_data$stations$station_abbr %in% 
            sub_inv$station_abbr, ]
        if (nrow(sub_inv) > 0) {
            data_since <- min(sub_inv$data_since)
            data_till <- max(sub_inv$data_till)
            wgs84_lat <- range(sub_stats$station_coordinates_wgs84_lat)
            wgs84_lon <- range(sub_stats$station_coordinates_wgs84_lon)
            parameters <- unique(sub_paras$parameter_shortname)
            stations <- unique(sub_stats$station_abbr)
        } else {
            data_since <- lubridate::NA_POSIXct_
            data_till <- NA_integer_
            wgs84_lat <- c(NA_real_, NA_real_)
            wgs84_lon <- c(NA_real_, NA_real_)
            parameters <- NA_character_
            stations <- NA_character_
        }
        structure(
            list(
                assets = meta_data$assets,
                datainventory = sub_inv,
                stations = sub_stats,
                parameters = sub_paras
            ), 
            class = 'met_metadata', 
            # pass collection
            collection = attr(meta_data, 'collection'),
            # update further attributes
            stations = stations,
            wgs84_lat = wgs84_lat,
            wgs84_lon = wgs84_lon,
            parameters = parameters,
            data_since = data_since,
            data_till = data_till,
            search_fromto = attr(meta_data, 'search_fromto'),
            search_location = attr(meta_data, 'search_location'),
            search_parameters = search_parameters
        )
    } else {
        out <- sapply(meta_data, search_by_parameter, shortname = shortname, 
            unit = unit, group = group, description = description, 
            language = language, granularity = granularity, 
            drop_nodata = drop_nodata, simplify = FALSE)
        if (drop_nodata) {
            out[!sapply(out, is.null)]
        } else {
            out
        }
    }
}


