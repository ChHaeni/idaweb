
# fetch available MeteoSwiss Open Data
# run to show all supported collections: collections(TRUE)

#' Fetch MeteoSwiss Open Data Collections of Ground-based Measurements
#'
#' Retrieves metadata for the most of the available ground-based measurement 
#' Open Data collections exposed via the MeteoSwiss STAC API. 
#'
#' @param set_name A character vector of collection short names (e.g.
#'   \code{"smn"}). If \code{NULL} (the default), all ground-based 
#'   collections available through this package are queried.
#'
#' @return An object of class \code{met_collections}: a character vector of
#'   collection IDs with a \code{collections} attribute containing the raw
#'   API responses.
#'
#' @details The default collections are: \code{smn}, \code{smn-precip},
#'   \code{smn-tower}, \code{nime}, \code{tot}, \code{pollen}, \code{obs},
#'   and \code{phenology}.
#'
#' @examples
#' \dontrun{
#' # get all collections and print to console for a quick overview
#' cols <- collections()
#' print(cols)
#' # automatic weather stations
#' cols <- collections("smn")
#' # automatic weather stations and manual precipitation stations
#' cols <- collections(c("smn", "nime"))
#' }
#' @export
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

#' Access Meta Data - Parameter Info
#'
#' Accessor function to extract the parameter table from a
#' \code{met_metadata} object (or a list of such objects).
#'
#' @param meta_data An object of class \code{met_metadata}, or a list of
#'   \code{met_metadata} objects.
#' @param as_dt Logical. Should the result be returned as a
#'   \code{data.table}? Default is \code{FALSE}.
#' @param cols Character vector of column names to retain. If \code{NULL}
#'   (default), all columns are returned.
#' @param uniq Logical. Should duplicate rows be removed? Defaults to
#'   \code{!is.null(cols)}.
#'
#' @return A \code{data.frame} (or \code{data.table} if \code{as_dt = TRUE})
#'   of class \code{met_parameters}, or a list when \code{meta_data} is a
#'   list.
#'
#' @examples
#' \dontrun{
#' meta <- met_search(from = "01.01.2020", to = "31.12.2020", group = "wind")
#' parameters(meta)
#' parameters(meta, cols = c("parameter_shortname", "parameter_description_en", 
#'   "parameter_unit"))
#' }
#' @export
parameters <- function(meta_data, as_dt = FALSE, cols = NULL, uniq = !is.null(cols)) {
    if (inherits(meta_data, 'met_metadata')) {
        out <- meta_data$parameters
    } else if (class(meta_data)[1] == 'list') {
        return(
            sapply(meta_data, parameters, as_dt = as_dt, cols = cols, uniq = uniq, 
                simplify = FALSE)
        )
    } else {
        out <- attr(meta_data, 'parameters')
    }
    # check cols
    if (!is.null(cols)) {
        out <- out[, cols]
        if (uniq) {
            out <- unique(out)
        }
    }
    # check as data.table
    if (as_dt) {
        out <- as.data.table(out)
    }
    out
}

#' Access Meta Data - Station Info
#'
#' Accessor function to extract the station table from a
#' \code{met_metadata} object (or a list of such objects).
#'
#' @param meta_data An object of class \code{met_metadata}, or a list of
#'   \code{met_metadata} objects.
#' @param as_dt Logical. Should the result be returned as a
#'   \code{data.table}? Default is \code{FALSE}.
#' @param cols Character vector of column names to retain. If \code{NULL}
#'   (default), all columns are returned.
#' @param uniq Logical. Should duplicate rows be removed? Defaults to
#'   \code{!is.null(cols)}.
#'
#' @return A \code{data.frame} (or \code{data.table} if \code{as_dt = TRUE})
#'   of class \code{met_stations}, or a list when \code{meta_data} is a
#'   list.
#'
#' @examples
#' \dontrun{
#' meta <- met_search(from = "01.01.2020", to = "31.12.2020", abbr = "BER")
#' stations(meta)
#' }
#' @export
stations <- function(meta_data, as_dt = FALSE, cols = NULL, uniq = !is.null(cols)) {
    if (inherits(meta_data, 'met_metadata')) {
        out <- meta_data$stations
    } else if (class(meta_data)[1] == 'list') {
        return(
            sapply(meta_data, stations, as_dt = as_dt, cols = cols, uniq = uniq, 
                simplify = FALSE)
        )
    } else {
        out <- attr(meta_data, 'stations')
    }
    # check cols
    if (!is.null(cols)) {
        out <- out[, cols]
        if (uniq) {
            out <- unique(out)
        }
    }
    # check as data.table
    if (as_dt) {
        out <- as.data.table(out)
    }
    out
}

#' Access Meta Data Inventory Info
#'
#' TODO
#'
#' @param meta_data \code{met_metadata}. TODO
#' @param as_dt logical. Default \code{FALSE}. TODO
#' @param cols character vector. Default \code{NULL}. TODO
#' @param uniq logical. Default \code{!is.null(cols)}. TODO
#' @return object of class \code{met_datainventory}. TODO
#' @export
datainventory <- function(meta_data, as_dt = FALSE, cols = NULL, uniq = !is.null(cols)) {
    if (inherits(meta_data, 'met_metadata')) {
        out <- meta_data$datainventory
        if (!is.null(cols)) {
            out <- out[, cols]
            if (uniq) {
                out <- unique(out)
            }
        }
        if (as_dt) {
            out <- as.data.table(out)
        }
    } else if (is.list(meta_data)) {
        out <- sapply(meta_data, datainventory, as_dt = as_dt, cols = cols, uniq = uniq, 
            simplify = FALSE)
    } else {
        stop('argument "meta_data" is not valid!')
    }
    out
}

## unexported ----------------------------------------

# get meta data
get_metadata <- function(id, type = c('datainventory', 'stations', 'parameters'),
    cache_dir = NULL) {
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
                    warning(file_name, ' has recent changes: ',
                        'metadata.rda in package needs updating!', immediate. = TRUE)
                    # ?
                }
                # download file
                cat('-> Fetching new data from MeteoSwiss - ')
                local_file <- .dl_data(file_url, cache_dir, file_checksum)
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
                # fix class
                class(out) <- c(paste0('met_', type), 'data.frame')
                # return
                return(out)
            }
            nm <- sub('.+_meta_(.+)[.]csv', '\\1', file_name)
            return(idaweb::metadata[[id]][[nm]])
        } else {
            warning('argument "id" is not recognized - returning NULL', immediate. = TRUE)
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

