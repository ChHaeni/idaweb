
## download data from search result ----------------------------------------

#' Download data from Meteoswiss (Open Data)
#'
#' This function downloads data from Meteoswiss and caches them locally. 
#' The data can be stored long-term, where the "cache" directory can be provided by the user.
#'
#' @param meta_data An object of class \code{met_metadata} - a collection of meta data on the data which should be downloaded.
#' @param cache_dir character. Path to the directory where the cached data is saved. Default is \code{tempdir()}.
#' @param single_timestamp logical. Should a single time stamp be given in the data (default), or should the start and end (named \code{'st'} and \code{'et'}) of each interval be provided?
#' @param outclass character. Class of the returned data. Default is \code{data.frame}. See \code{Details} on how data is provided.
#' @param outstruc character. How should the returned object be structured? Default is \code{'split-all'}. See \code{Details} for available options.
#' @return Meteoswiss data provided as a \code{list}, except if argument \code{outstruc} is set to \code{'cbind-all'}. If argument \code{meta_data} is provided as a list of meta data (e.g. like the default \code{idaweb::metadata}), the returned object will have the same list structure on the top level (see \code{Details}).
#' @details 
#'  Argument \code{outstruc} can be used to structure the output in four different ways:
#'  \itemize{
#'      \item{\code{'split-all'} - (default) a \code{list} with each entry representing unique combinations of station/granularity.}
#'      \item{\code{'by-station'} - a \code{list}. Each list entry represents data gathered over all granularities for each unique station.}
#'      \item{\code{'by-granularity'} - a \code{list}. Each list entry represents data gathered over all stations for each unique granularity.}
#'      \item{\code{'cbind-all'} - All data is gathered into a single object.}
#'  }
#'  The lowest level data is of the class provided with \code{outclass} (default is \code{data.frame}).
#' If argument \code{meta_data} is provided as a list of meta data (e.g. like the default \code{idaweb::metadata}), the returned object will have the same list structure on the top level (see \code{Details}).
#' @examples
#'  \dontrun{
#'  require(idaweb)
#'  # search for wind data
#'  meta_wind <- met_search('01.01.2025 to 04.02.2026', lon = '7.4..7.5', 
#'      lat = '46.9..47.3', granularity = c('d', 'm'), group = 'wind')
#'  # if meta data is provided as list, the returned object will be a list, too
#'  data_wind1 <- get_data(meta_wind[[1]])
#'  data_wind2 <- get_data(meta_wind, outstruc = 'by-station', outclass = 'data.table')
#'  data_wind3 <- get_data(meta_wind[[1]], outstruc = 'cbind-all', outclass = 'data.table')
#'  data_wind4 <- get_data(meta_wind, outclass = 'ibts')
#'  }
#' @export
get_data <- function(meta_data, cache_dir = tempdir(), single_timestamp = TRUE, 
    outclass = c('data.frame', 'data.table', 'ibts', 'df', 'dt'),
    outstruc = c('split-all', 'by-station', 'by-granularity', 'cbind-all')
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
            single_timestamp = single_timestamp)
    } else {
        # loop over list
        sapply(meta_data, get_data, cache_dir = cache_dir, outclass = outclass,
            outstruc = outstruc, single_timestamp = single_timestamp, 
            simplify = FALSE)
    }
}

