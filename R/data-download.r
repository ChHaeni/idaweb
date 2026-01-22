
## download data from search result ----------------------------------------

get_data <- function(meta_data, cache_dir = tempdir(), as_DT = TRUE, 
    force_cache = FALSE) {
    if (inherits(meta_data, 'met_metadata')) {
        # get filenames etc.
        meta_data <- .get_filenames(meta_data)
    }
    if (inherits(meta_data, 'file_list')) {
        # get files
        meta_data <- .get_files(meta_data, cache_dir = cache_dir, 
            force_cache = force_cache)
    }
    if (inherits(meta_data, 'dl_files')) {
        # get data from files
        .get_data(meta_data, as_DT = as_DT)
    } else {
        # loop over list
        sapply(meta_data, get_data, cache_dir = cache_dir, as_DT = as_DT,
            force_cache = force_cache, simplify = FALSE)
    }
}

