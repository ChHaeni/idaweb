


## NOTES ----------------------------------------

# REST API docu
# https://data.geo.admin.ch/api/stac/static/spec/v1/api.html#tag/Data

# only collections from meteoswiss
# only ids containing ogd (because of meta data & data format)
# check data sets: https://opendatadocs.meteoswiss.ch/

# ground-base measurements only
# data sets A1 to A9
# smn, smn-preicp, smn-tower, nime, tot, pollen, obs, phenology

# note on data handling:
# mail from support: Re: Incident INC000002367960 / Daten 2025 unter "aktuelles Jahr"
#   Die Daten 2025 sind bis im Februar noch unter «aktuelles Jahr» zu finden.
#   Danach werden die Daten geprüft und definitiv und unter 2020-2929 auffindbar sein.

# overview on functions:
# -

# TODO:
# - add option to convert to ibts
# - fetch data info
# - meta data included in package -> check if update needed
# - function to update specific or all meta data (if necessary)
# - download data only if not available in options
# - one function to get data (incl. options check)
# - show_on_map() => visualize subset on map
# - convenience functions:
#       * fix info()


##  • main functions ====================

# fetch available MeteoSwiss Open Data
# run to show all supported collections: collections(TRUE)
collections <- function(set_name = NULL) {
    if (is.list(set_name)) {
        set_names <- sub('.*\\.ogd-', '', names(set_name))
    } else if (!is.null(set_name) && is.character(set_name)) {
        set_names <- set_name
    } else {
        # all ground-base measurement sets
        set_names <- c('smn', 'smn-precip', 'smn-tower', 'nime', 'tot', 
            'pollen', 'obs', 'phenology')
    }
    # loop over sets
    out <- lapply(set_names, \(x) {
        # fix url
        get_url <- met_url('api/stac/v1/collections/ch.meteoschweiz.ogd-', x)
        # get info
        httr::content(httr::GET(get_url))
    })
    # get ids
    ids <- sapply(out, '[[', 'id')
    attr(ids, 'collections') <- out
    structure(ids, class = 'met_collections')
}

# # function to get info on specific data set(s) from collection
# # e.g.: info(collections())
# # x -> either met_collections object or a valid id
# info <- function(x, i = NULL) {
# }

# get meta data
get_metadata <- function(id, type = c('datainventory', 'stations', 'parameters'),
    cache_dir = tempdir()) {
    type <- match.arg(type)
    if (length(id) == 1L) {
        # check supported id
        if (i_supp <- check_supported(id)) {
            # get collection info
            id_coll <- attr(i_supp, 'collection')
            # get file update
            file_name <- grep(type, names(id_coll$assets), value = TRUE, 
                fixed = TRUE)
            last_updated <- lubridate::fast_strptime(
                id_coll$assets[[file_name]]$updated,
                format = "%Y-%m-%dT%H:%M:%OSZ", lt = FALSE
            )
            # check if package data is up-to-date
            meta_last_updated <- lubridate::fast_strptime(
                idaweb::metadata[[id]]$assets[[file_name]]$updated,
                format = "%Y-%m-%dT%H:%M:%OSZ", lt = FALSE
            )
            if (last_updated > meta_last_updated) {
                # update data...
                # get url & checksum
                file_url <- id_coll$assets[[file_name]]$href
                file_checksum <- id_coll$assets[[file_name]][['file:checksum']]
                if (!(basename(file_url) %in% names(.Options))) {
                    # TODO: add message about github issue (only first time if local
                    #           file does not exist yet)
                    # cat()
                    warning(file_name, ' has recent changes:',
                        'metadata.rda in package needs updating!')
                    # ?
                }
                # download file
                local_file <- .dl_data(file_url, file_checksum, cache_dir = cache_dir)
                out <- read.table(local_file, sep = ';', header = TRUE, fill = TRUE,
                        fileEncoding = 'Windows-1252', comment.char = '', quote = '"'
                    )
                # convert datetimes
                switch(type,
                    datainventory = {
                        out$data_since <- lubridate::fast_strptime(out$data_since, 
                            format = '%d.%m.%Y %H:%M', tz = 'UTC', lt = FALSE)
                        if (is.logical(out$data_till)) {
                            out$data_till <- as.POSIXct(out$data_till)
                        } else {
                            out$data_till <- lubridate::fast_strptime(out$data_till, 
                                format = '%d.%m.%Y %H:%M', tz = 'UTC', lt = FALSE)
                        }
                    },
                    stations = {
                        out$station_data_since <- lubridate::fast_strptime(
                            out$station_data_since, format = '%d.%m.%Y', 
                            tz = 'UTC', lt = FALSE)
                    }
                )
                return(out)
            }
            nm <- sub('.+_meta_(.+)[.]csv', '\\1', file_name)
            return(idaweb::metadata[[id]][[nm]])
        } else {
            return(invisible(NULL))
        }
    } else {
        # get data
        out <- lapply(id, get_metadata, type = type, cache_dir = cache_dir)
        names(out) <- id
        # remove invalid
        return(out[!sapply(out, is.null)])
    }
}

parameters <- function(meta_data, cols = NULL, uniq = !is.null(cols)) {
    if (inherits(meta_data, 'met_metadata')) {
        out <- meta_data$parameters
        if (!is.null(cols)) {
            out <- out[, cols]
            if (uniq) {
                out <- unique(out)
            }
        }
    } else if (is.list(meta_data)) {
        out <- sapply(meta_data, parameters, cols = cols, uniq = uniq, 
            simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
    out
}
stations <- function(meta_data) {
    if (inherits(meta_data, 'met_metadata')) {
        meta_data$stations
    } else if (is.list(meta_data)) {
        sapply(meta_data, stations, simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
}
datainventory <- function(meta_data) {
    if (inherits(meta_data, 'met_metadata')) {
        meta_data$datainventory
    } else if (is.list(meta_data)) {
        sapply(meta_data, datainventory, simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
}

# add option to provide previous results for further subsetting
# add function to bind different results together
# add function to get data from results


# "fuzzy" searching strings
fuzzy_search <- function(pattern, string, value = FALSE, return_logical = FALSE,
    ignore.case = all(is.na(pmatch(LETTERS, pattern)))) {
    fuz_pat <- paste(c('', unlist(strsplit(pattern, split = '')), ''), collapse = '.*')
    if (return_logical) {
        grepl(fuz_pat, string, ignore.case = ignore.case)
    } else {
        grep(fuz_pat, string, value = value, ignore.case = ignore.case)
    }
}

## re-build meta data ----------------------------------------

if (FALSE) {
    # TODO: make function to update metadata in package data path
    #       -> function to get package path: system.file(package=)
    #       -> name metadata data differently and check if exists in code
    # check collections from MeteoSwiss
    sup <- collections()
    sup

    # get meta data
    load('~/repos/3_Scripts/8_meteoswiss/data/metadata.rda')
    path_cache <- '~/repos/3_Scripts/8_meteoswiss/cached'
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

