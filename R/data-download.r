
## download data from search result ----------------------------------------

#' Download data from Meteoswiss (Open Data)
#'
#' This function downloads ground-based measurement data from the MeteoSwiss Open Data
#' API based on a \code{met_metadata} search result and caches them locally. 
#' The cache directory can be provided by the user to allow for permanent storage of 
#' the downloaded data.
#'
#' @param meta_data A \code{met_metadata} object (or a list of such objects)
#'   describing the data to be downloaded.
#' @param cache_dir Path to the local cache directory. Default is the temporary
#'   directory \code{\link[base]{tempdir()}.
#' @param single_timestamp Logical. If \code{TRUE} (default), a single
#'   \code{time} column is returned. If \code{FALSE}, the interval is
#'   represented by \code{st} (start) and \code{et} (end) columns.
#' @param outclass Desired class of the output objects. One of
#'   \code{"data.frame"} (default), \code{"data.table"}, \code{"ibts"},
#'   \code{"df"}, or \code{"dt"}. The latter two are abbreviations for the
#'   first two. See \code{Details} on how data is provided.
#' @param outstruc Desired structure of the returned object. One of
#'   \code{"split-all"} (default, each station/granularity combination is a
#'   separate list element), \code{"by-station"} (data aggregated per
#'   station), \code{"by-granularity"} (data aggregated per granularity), or
#'   \code{"cbind-all"} (single object, not a list). See \code{Details}.
#' @param tzone Time zone for the returned timestamps. Default is \code{"UTC"}.
#' @return
#' A list of data objects (except when \code{outstruc = "cbind-all"}, in
#' which case a single object is returned). The class of each element is
#' determined by \code{outclass}. If \code{meta_data} is a list, the top
#' level of the return value mirrors that list structure.
#'
#' Each data object carries two attributes:
#' \itemize{
#'   \item \code{parameters} – the parameter metadata used.
#'   \item \code{stations} – the station metadata used.
#' }
#'
#' @details 
#' The MeteoSwiss Open Data files are organised by station and granularity.
#' Depending on the granularity and time period, different CSV files have to
#' be fetched (historical, recent, now). This function handles the file
#' selection, download, and parsing transparently.
#'
#'  Argument \code{outstruc} can be used to structure the output in four different ways:
#'  \itemize{
#'      \item{\code{'split-all'} - (default) a \code{list} with each entry representing unique combinations of station/granularity.}
#'      \item{\code{'by-station'} - a \code{list}. Each list entry represents data gathered over all granularities for each unique station.}
#'      \item{\code{'by-granularity'} - a \code{list}. Each list entry represents data gathered over all stations for each unique granularity.}
#'      \item{\code{'cbind-all'} - All data is gathered into a single object.}
#'  }
#'  The final data is of the class provided with \code{outclass} (default is \code{data.frame}).
#' If argument \code{meta_data} is provided as a list of meta data (e.g. like the default \code{idaweb::metadata}), the returned object will have the same list structure on the top level (see \code{Details}).
#'
#' When \code{outclass = "ibts"}, the optional \pkg{ibts} package is
#' required. This output structure forces \code{outstruc = "split-all"} and
#'
#' @seealso \code{\link{met_search}}, \code{\link{parameters}},
#'   \code{\link{stations}}
#' \code{single_timestamp = FALSE}.
#'
#' @examples
#' \dontrun{
#' # Search for wind data
#' meta_wind <- met_search(
#'   from = "01.01.2025", to = "04.02.2026",
#'   lon = "7.4..7.5", lat = "46.9..47.3",
#'   granularity = c("D", "M"), group = "wind"
#' )
#'
#' # Download a single metadata entry
#' data_wind1 <- get_data(meta_wind[[1]])
#'
#' # Download all entries, aggregated by station, as data.table
#' data_wind2 <- get_data(
#'   meta_wind,
#'   outstruc = "by-station",
#'   outclass = "data.table"
#' )
#'
#' # Single combined data.table
#' data_wind3 <- get_data(
#'   meta_wind[[1]],
#'   outstruc = "cbind-all",
#'   outclass = "data.table"
#' )
#' }
#' @export
get_data <- function(meta_data, cache_dir = tempdir(), single_timestamp = TRUE, 
    outclass = c('data.frame', 'data.table', 'ibts', 'df', 'dt'),
    outstruc = c('split-all', 'by-station', 'by-granularity', 'cbind-all'),
    tzone = 'UTC'
    ) {
    # check arguments
    outclass <- match.arg(outclass)
    outstruc <- match.arg(outstruc)
    if (inherits(meta_data, 'met_metadata')) {
        # get filenames etc.
        meta_data <- .get_filenames(meta_data)
    }
    if (inherits(meta_data, 'file_list')) {
        # get files
        meta_data <- .get_files(meta_data, cache_dir)
    }
    if (inherits(meta_data, 'dl_files')) {
        # get data from files
        .get_data(meta_data, outclass = outclass, outstruc = outstruc,
            single_timestamp = single_timestamp, tzone = tzone)
    } else {
        # loop over list
        sapply(meta_data, get_data, cache_dir = cache_dir, outclass = outclass,
            outstruc = outstruc, single_timestamp = single_timestamp, 
            tzone = tzone, simplify = FALSE)
    }
}

