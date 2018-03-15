#' @title Import CSVY data
#' @description Import CSVY data as a data.frame
#' @param file A character string or R connection specifying a file.
#' @param metadata Optionally, a character string specifying a YAML (\dQuote{.yaml}) or JSON (\dQuote{.json}) file containing metadata (in lieu of including it in the header of the file).
#' @param stringsAsFactors A logical specifying whether to treat character columns as factors. Passed to \code{\link[utils]{read.csv}} or \code{\link[data.table]{fread}} depending on the value of \code{method}. Ignored for \code{method = 'readr'} which never returns factors.
#' @param \dots Additional arguments passed to \code{\link[data.table]{fread}}.
#' @examples
#' read_csvy(system.file("examples", "example3.csvy", package = "csvy"))
#' 
#' @importFrom tools file_ext
#' @importFrom jsonlite fromJSON
#' @importFrom data.table fread
#' @importFrom yaml yaml.load
#' @export
#' @seealso \code{\link{write_csvy}}
read_csvy <-
function(
    file,
    metadata = NULL,
    stringsAsFactors = FALSE,
    ...
) {
    
    # setup factor coercion conditional on presence of 'levels' metadata field
    if (isTRUE(stringsAsFactors)) {
        try_to_factorize <- "always"
    } else if (stringsAsFactors == "conditional") {
        stringsAsFactors <- FALSE
        try_to_factorize <- "conditional"
    } else {
        try_to_factorize <- "never"
    }
        
    if (is.null(metadata)) {
        metadata_raw <- get_yaml_header(file, verbose = FALSE)
        if (is.null(metadata_raw)) {
            message("No yaml delimiters found. Reading file as CSV.")
            out <- data.table::fread(input = file, sep = "auto", header = "auto",
                                     stringsAsFactors = stringsAsFactors,
                                     data.table = FALSE, ...)
            return(out)
        }
        skip_lines <- length(metadata_raw) + 1L     # Including opening and closing "---"
        metadata_list <- yaml::yaml.load(paste(metadata_raw, collapse = "\n"))
    } else {
        ext <- tools::file_ext(metadata)
        skip_lines <- 0L
        if (ext == "yaml") {
            metadata_list <- yaml::yaml.load(paste(readLines(metadata), collapse = "\n"))
        } else if (ext == "json") {
            metadata_list <- jsonlite::fromJSON(metadata, simplifyDataFrame = FALSE)
        } else {
            warning("'metadata' should be either a .json or .yaml file.")
        }
    }
    
    # find variable-level metadata 'fields'
    if ("fields" %in% names(metadata_list)) {
        # this is a legacy
        fields <- metadata_list$fields
    } else if ("resources" %in% names(metadata_list)) {
        # this is the current standard
        # get first resource field (currently we don't support multiple resources)
        fields <- metadata_list$resources[[1L]]$schema$fields
        field_types <- vapply(fields, "[[", character(1), "type")
        col_classes <- colclass_dict[field_types]
        names(col_classes) <- vapply(fields, "[[", character(1), "name")
    } else {
        fields <- NULL
        col_classes <- NULL
    }
    
    # find 'dialect' to use for importing, if available
    if ("resources" %in% names(metadata_list)) {
        dialect <- metadata_list$resources[[1L]]$dialect
        ## delimiter
        sep <- dialect$delimeter
        if (is.null(sep)) sep <- "auto"
        ## header
        header <- as.logical(dialect$header)
        if (is.null(header)) header <- "auto"
        ## there are other args here but we really don't need them
        ## need to decide how to use them
    } else {
        sep <- "auto"
        header <- "auto"
    }

    # load the data
    out <- data.table::fread(
        file = file,
        sep = sep,
        header = header,
        stringsAsFactors = stringsAsFactors,
        data.table = FALSE,
        colClasses = col_classes,
        skip = skip_lines,
        ...
    )
    
    # add data frame-level metadata to data
    out <- add_dataset_metadata(out, metadata_list)
    
    # add variable-level metadata to data
    out <- add_variable_metadata(out, fields, try_to_factorize = try_to_factorize)
    
    return(out)
}
