
## download data from search result ----------------------------------------

#' Download data from Meteoswiss (Open Data)
#'
#' @param meta_data An object of class \code{met_metadata} - a collection of meta data on the data which should be downloaded.
#' @param cache_dir character. Path to the directory where the cached data is saved.
#' @param force_cache logical. Should the argument \code{cache_dir} be taken, even if the cached data has been saved elsewhere during the current R session? Default is \code{FALSE}.
#' @param single_timestamp logical. Should a single time stamp be given in the data (default), or should the start and end (named \code{\sQuote{st}} and \code{\sQuote{et}}) of each interval be provided?
#' @param outclass character. Class of the returned data. Default is \code{data.frame}. See \code{Details} on how data is provided.
#' @param outstruc character. How should the returned object be structured? Default is \code{\sQuote{split-all}}. See \code{Details} for available options.
#' @return Meteoswiss data provided as a \code{list}, except if argument \code{outstruc} is set to \code{\sQuote{cbind-all}} (see \code{Details}).
#' @details 
#'  argument \code{outstruc} can be used to structure the output in four different ways.
#'  \describe{
#'      \item{\code{\sQuote{split-all}}}{(default) a \code{list} with each entry representing unique combinations of station/granularity.}
#'      \item{\code{\sQuote{by-station}}}{a \code{list}. Each list entry represents data gathered over all granularities for each unique station.}
#'      \item{\code{\sQuote{by-granularity}}}{a \code{list}. Each list entry represents data gathered over all stations for each unique granularity.}
#'      \item{\code{\sQuote{cbind-all}}}{All data is gathered into a single object.}
#'  }
#'  The lowest level data is of the class provided with \code{outclass} (default is \code{data.frame}).
#' @export
get_data <- function(meta_data, cache_dir = tempdir(), force_cache = FALSE, 
    single_timestamp = TRUE, outclass = c('data.frame', 'data.table', 'ibts', 'df', 'dt'),
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
        meta_data <- .get_files(meta_data, cache_dir, force_cache = force_cache)
    }
    if (inherits(meta_data, 'dl_files')) {
        # get data from files
        .get_data(meta_data, outclass = outclass, outstruc = outstruc,
            single_timestamp = single_timestamp)
    } else {
        # loop over list
        sapply(meta_data, get_data, cache_dir = cache_dir, outclass = outclass,
            outstruc = outstruc, single_timestamp = single_timestamp, 
            force_cache = force_cache, simplify = FALSE)
    }
}

