
## search wrapper (main function) ----------------------------------------

#' Search MeteoSwiss Open Data
#'
#' Flexible search interface to filter MeteoSwiss ground-based measurement
#' metadata by date/time, location, and/or parameter. The function chains
#' \code{\link{search_by_location}}, \code{\link{search_by_parameter}}, and
#' \code{\link{search_by_datetime}} according to the arguments provided.
#'
#' @param from Start date/time. Can be a \code{character} string (e.g.
#'   \code{"01.01.2020"} or \code{"01.01.2020 to 31.12.2020"}) or a
#'   \code{POSIXt} object.
#' @param to End date/time. Can be a \code{character} string or a
#'   \code{POSIXt} object. If omitted, an open-ended interval is assumed.
#' @param tz Time zone for \code{from} and \code{to}. Default is inferred
#'   from the inputs or falls back to \code{"UTC"}.
#' @param lon Longitudinal range. Accepts WGS84 (approx. 5\u00B0–11\u00B0) or Swiss
#'   coordinate values. Ranges can be given as \code{"7.4..7.5"},
#'   \code{"7.4 to 7.5"}, or a numeric vector \code{c(7.4, 7.5)}.
#' @param lat Latitudinal range. Accepts WGS84 (approx. 45\u00B0–48\u00B0) or Swiss
#'   coordinate values (see \code{lon}).
#' @param alt Altitude range in metres a.s.l. Same range format as \code{lon}/\code{lat}
#'   or a numeric vector. Use \code{-Inf}/\code{Inf} for either side open limit.
#' @param abbr Station abbreviation(s) as three-letter character vector(s)
#'   (e.g. \code{"BER"}).
#' @param name Character string for fuzzy matching against station names.
#'   Prefix with \code{=} to force non-fuzzy (partial) matching 
#'   (e.g. \code{uetl} vs. \code{=uetl}).
#' @param canton Character vector of canton abbreviations (e.g. \code{"BE"}).
#' @param shortname Parameter short name(s) to search for (partial matching
#'   via \code{\link[base]{grep}}).
#' @param unit Parameter unit(s) to search for (partial matching).
#' @param group Parameter group(s) to search for (fuzzy matching). The search
#'   is language-dependent.
#' @param description Parameter description(s) to search for (fuzzy matching).
#'   The search is language-dependent.
#' @param granularity Temporal granularity(s) to include: \code{"T"} (10 min),
#'   \code{"H"} (hourly), \code{"D"} (daily), \code{"M"} (monthly),
#'   \code{"Y"} (yearly). Default is all. Case insensitive.
#' @param language Language for \code{group} and \code{description} matching:
#'   \code{"en"} (default), \code{"de"}, \code{"fr"}, or \code{"it"}.
#' @param meta_data A \code{met_metadata} object or list thereof. Defaults to
#'   the package dataset \code{\link[idaweb]{metadata}}.
#' @param drop_nodata Logical. Should collections with no matching data be
#'   dropped from the result? Default is \code{TRUE}.
#'
#' @return A \code{met_metadata} object, or a named list of such objects if
#'   \code{meta_data} was a list.
#'
#' @details
#' The search is performed in three consecutive steps:
#' \enumerate{
#'   \item Location filtering (\code{lon}, \code{lat}, \code{alt}, \code{abbr},
#'     \code{name}, \code{canton}).
#'   \item Parameter filtering (\code{shortname}, \code{unit}, \code{group},
#'     \code{description}, \code{granularity}, \code{language}).
#'   \item Date/time filtering (\code{from}, \code{to}, \code{tz}).
#' }
#' At each step the result is passed to the next filter, allowing very
#' specific queries.
#'
#' Swiss coordinates (LV03 / LV95) are automatically detected and converted
#' to WGS84 using the \pkg{sf} package when needed.
#'
#' @seealso \code{\link{search_by_location}}, \code{\link{search_by_parameter}},
#'   \code{\link{search_by_datetime}}
#'
#' @examples
#' \dontrun{
#' # Daily averages of air temperature at Zollikofen
#' mtemp <- met_search(
#'   from = "12.08.2014 to 02.02.2026",
#'   granularity = "D",
#'   lon = "7.43..7.49", lat = "46.96..47.12",
#'   group = "Temperature"
#' )
#' 
#' # Further subset: 2 m mean air temperature
#' mfinal <- met_search(
#'   description = "2mmean",
#'   meta_data = mtemp
#' )
#'
#' # Wind data in central Switzerland, daily & monthly
#' mwind <- met_search(
#'   from = "01.01.2025", to = "04.02.2026",
#'   lon = "7.4..7.5", lat = "46.9..47.3",
#'   granularity = c("D", "M"), group = "wind"
#' )
#'
#' # Exact station and parameter
#' mexact <- met_search(abbr = "BER", shortname = "tre200s0")
#' }
#' @export
met_search <- function(
    # by datetime
    from, to, tz = get_tzone(from, to), 
    # by location TODO: change name to station? or station_name and allow short form (->match.args())
    lon, lat, alt, abbr, name, canton,
    # by parameter TODO: change shortname to parameter? or parameter_name and allow short form?
    shortname, unit, group, description, 
    granularity = c('T', 'H', 'D', 'M', 'Y'), 
    language = c('en', 'de', 'fr', 'it'), 
    # all
    meta_data = idaweb::metadata, drop_nodata = TRUE
) {
    # first search by location
    if (!all(missing(lon), missing(lat), missing(alt), missing(abbr),
            missing(name), missing(canton))) {
        meta_data <- search_by_location(lon = lon, lat = lat, alt = alt, abbr = abbr, 
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

#' Filter Metadata by Date and Time
#'
#' Subsets a \code{met_metadata} object (or a list thereof) by temporal coverage.
#' Only station-parameter combinations whose data inventory overlaps with the
#' requested interval are retained.
#'
#' @param from Start date/time. See \code{\link{met_search}}.
#' @param to End date/time. See \code{\link{met_search}}.
#' @param tz Time zone for \code{from} and \code{to}. See \code{\link{met_search}}.
#' @param meta_data A \code{met_metadata} object or list thereof. Defaults to
#'   \code{\link[idaweb]{metadata}}.
#' @param drop_nodata Logical. If \code{TRUE}, collections with an empty
#'   inventory after filtering are removed.
#'
#' @return A \code{met_metadata} object or a list thereof.
#'
#' @seealso \code{\link{search_by_location}}, \code{\link{search_by_parameter}}
#'
#' @examples
#' \dontrun{
#' meta <- search_by_datetime("01.01.2020", "31.12.2020")
#' meta <- search_by_datetime("01.01.2020", 
#'   meta_data = idaweb::metadata[[1]])
#' }
#' @export
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

#' Filter Metadata by Location
#'
#' Subsets a \code{met_metadata} object (or list thereof) based on geographic or
#' administrative criteria.
#'
#' @param lon Longitudinal range. See \code{\link{met_search}}.
#' @param lat Latitudinal range. See \code{\link{met_search}}.
#' @param alt Altitude range in metres. See \code{\link{met_search}}.
#' @param abbr Exact station abbreviation(s).
#' @param name Fuzzy station name pattern.
#' @param canton Exact canton abbreviation(s).
#' @param meta_data A \code{met_metadata} object or list thereof. Defaults to
#'   \code{\link[idaweb]{metadata}}.
#' @param drop_nodata Logical. If \code{TRUE}, collections with an empty
#'   inventory after filtering are removed.
#'
#' @return A \code{met_metadata} object or a list thereof.
#'
#' @details
#' Coordinates are interpreted as WGS84 when values fall into typical
#' longitude/latitude ranges. Swiss coordinates (LV03 or LV95) are
#' automatically converted to WGS84 using \pkg{sf} if that package is
#' installed. \code{lon} and \code{lat} must be provided pairwise.
#'
#' Station names are matched with a fuzzy search algorithm unless the
#' pattern is prefixed with \code{=}, then they are partially matched.
#'
#' @seealso \code{\link{search_by_datetime}}, \code{\link{search_by_parameter}}
#'
#' @examples
#' \dontrun{
#' meta <- search_by_location(lon = "7.4..7.5", lat = "46.9..47.3")
#' meta <- search_by_location(abbr = c("BER", "ZUE"))
#' meta <- search_by_location(name = "Zurich")
#' meta <- search_by_location(canton = "BE")
#' # Compare the following (case insensitive)
#' # 6 station names match by fuzzy search (i.e. any sequence of .*b.*e.*r.*n.*)
#' m1 <- search_by_location(name = "bern", meta_data = idaweb::metadata[[1]])
#' # 4 station names match by partial matching (i.e. matching .*bern.*)
#' m2 <- search_by_location(name = "=bern", meta_data = idaweb::metadata[[1]])
#' # 1 station name matches that starts with bern (i.e. matching ^bern.*)
#' m3 <- search_by_location(name = "=^bern", meta_data = idaweb::metadata[[1]])
#' }
#' @export
search_by_location <- function(lon, lat, alt, abbr, name, canton, 
    meta_data = idaweb::metadata, drop_nodata = FALSE) {
    # valid search entries:
    # lat & lon: '46.1..46.2', '46.1 to 46.2', '46.1/46.2', c(46.1, 46.2), 
    # TODO: ch_x & ch_y: same as above BUT additionally, only 100-thousands 
    #   -> distinguish between lv95 and lv03
    #   -> check lon/lat as required in R and possibly flip
    # only attach sf if really necessary
    # fix meta argument
    meta_data <- fix_meta_arg(meta_data)
    # parse lon & lat
    xy <- fix_wgs84(lon, lat)
    # reassign back
    xv <- lapply(xy, '[[', 1)
    yv <- lapply(xy, '[[', 2)
    # parse alt
    zv <- check_z_arg(alt)
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
        if (is.null(xv) || length(xv) == 0) {
            i_x <- rep(TRUE, n_stations)
        } else {
            # subset by longitude
            s_lon <- meta_data$stations$station_coordinates_wgs84_lon 
            i_x <- unlist(lapply(xv, \(v) s_lon >= v[1] & s_lon <= v[2]))
            search_location <- c(search_location, list(lon = lon))
        }
        # check y/lat
        if (is.null(yv) || length(yv) == 0) {
            i_y <- rep(TRUE, n_stations)
        } else {
            # subset by latitude
            s_lat <- meta_data$stations$station_coordinates_wgs84_lat 
            i_y <- unlist(lapply(yv, \(v) s_lat >= v[1] & s_lat <= v[2]))
            search_location <- c(search_location, list(lat = lat))
        }
        # check z/elevation
        if (is.null(zv) || length(zv) == 0) {
            i_z <- rep(TRUE, n_stations)
        } else {
            # subset by elevation
            s_el <- meta_data$stations$station_height_masl
            i_z <- unlist(lapply(zv, \(v) s_el >= v[1] & s_el <= v[2]))
            search_location <- c(search_location, list(alt = alt))
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
        out <- sapply(meta_data, search_by_location, lon = xv, lat = yv, 
            alt = zv, abbr = abbr, name = name,
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

#' Filter Metadata by Parameter
#'
#' Subsets a \code{met_metadata} object (or list) based on measurement
#' parameter characteristics.
#'
#' @param shortname Parameter short name(s) to grep for.
#' @param unit Parameter unit(s) to grep for.
#' @param group Parameter group(s) to fuzzy-match (e.g. \code{"wind"},
#'   \code{"temperature"}). Language-dependent.
#' @param description Parameter description(s) to fuzzy-match.
#'   Language-dependent. Prefix with \code{=} for non-fuzzy (partial) matching.
#' @param granularity Temporal granularity(s): \code{"T"}, \code{"H"},
#'   \code{"D"}, \code{"M"}, \code{"Y"}. Default is all.
#' @param language Language for \code{group} and \code{description} labels:
#'   \code{"en"} (default), \code{"de"}, \code{"fr"}, or \code{"it"}.
#' @param meta_data A \code{met_metadata} object or list thereof. Defaults to
#'   \code{\link[idaweb]{metadata}}.
#' @param drop_nodata Logical. If \code{TRUE}, collections with an empty
#'   inventory after filtering are removed.
#'
#' @return A \code{met_metadata} object or a list thereof.
#'
#' @seealso \code{\link{search_by_datetime}}, \code{\link{search_by_location}}
#'
#' @examples
#' \dontrun{
#' meta <- search_by_parameter(group = "precipitation", granularity = "D")
#' meta <- search_by_parameter(shortname = "tre200s0")
#' }
#' @export
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


