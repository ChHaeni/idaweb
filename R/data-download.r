
## download data from search result ----------------------------------------

get_data <- function(meta_data, cache_dir = tempdir(), force_cache = FALSE, 
    outclass = c('data.frame', 'data.table', 'ibts', 'df', 'dt'), single_timestamp = TRUE,
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

