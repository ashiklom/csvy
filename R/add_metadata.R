#' Add metadata to columns in a data frame
#'
#' @param data Data frame whose columns should be modified
#' @param fields Named list of metadata fields to add to data
#' @param try_to_factorize Whether or not to coerce to factor
#' @return Modified data object with attributes added to columns 
add_variable_metadata <- function(data, fields, try_to_factorize = "never") {

    # check metadata against header row
    check_variable_metadata(data = data, fields = fields)
    
    # add metadata to data, iterating across metadata list
    metadata_names <- vapply(fields, "[[", character(1), "name")
    for (i in seq_along(fields)) {
        # grab attributes for this variable
        field <- fields[[i]]
        column <- metadata_names[i]
        attrs <- setdiff(names(field), c("names", "class"))
        for (attribute in attrs) {
            data[[column]] <- attr_from_field(data[[column]], attribute, field, try_to_factorize)
        }
    }
    
    return(data)
}

#' Add metadata to a dataset as a whole
#'
#' @inheritParams add_variable_metadata
#' @param metadata_list List of metadata fields to add to data
#' @return data Modified data object with attributes added to columns
add_dataset_metadata <- function(data, metadata_list) {
    # Remove metadata objects already used.
    metadata_list[c("fields", "resources")] <- NULL
    for (attribute in names(metadata_list)) {
        data <- attr_from_field(data, attribute, metadata_list)
    }
    return(data)
}

check_variable_metadata <- function(data, fields) {
    if (is.null(fields)) {
        return(NULL)
    }
    
    hnames <- lapply(fields, `[[`, "name")
    
    missing_from_metadata <- names(data)[!names(data) %in% hnames]
    if (length(missing_from_metadata)) {
        warning("Metadata is missing for ", 
                ngettext(length(missing_from_metadata), "variable", "variables"), 
                " listed in data: ", paste(missing_from_metadata, collapse = ", "))
    }
    
    missing_from_data <- unlist(hnames)[!unlist(hnames) %in% names(data)]
    if (length(missing_from_data)) {
        warning("Data is missing for ", 
                ngettext(length(missing_from_data), "variable", "variables"), 
                " listed in frontmatter: ", paste(missing_from_metadata, collapse = ", "))
    }
    
    duplicated_metadata <- unlist(hnames)[duplicated(unlist(hnames))]
    if (length(duplicated_metadata)) {
        warning("Duplicate metadata entries for ", 
                ngettext(length(duplicated_metadata), "variable", "variables"), 
                " listed in frontmatter: ", paste(duplicated_metadata, collapse = ", "))
    }
    
    duplicated_columns <- unlist(hnames)[duplicated(unlist(hnames))]
    if (length(duplicated_columns)) {
        warning("Duplicate column names for ", 
                ngettext(length(duplicated_columns), "variable", "variables"), 
                ": ", paste(duplicated_metadata, collapse = ", "))
    }
    
    NULL
}

#' Set an object's attribute from a list object
#'
#' Note that the "type" attribute is a special case.
#' @param variable Object to be modified
#' @param attribute Character string of attribute to apply to variable
#' @param fields Named list of fields from which to get attribute
#' @inheritParams add_variable_metadata
#' @return variable, with additional attribute added
attr_from_field <- function(variable, attribute, fields, try_to_factorize = NULL) {
    if (attribute %in% names(fields)) {
        if (attribute == "type") {
            variable <- type_from_field(variable, fields, try_to_factorize)
        } else if (!attribute %in% names(attributes(variable))) {
            attr(variable, attribute) <- fields[[attribute]]
        }
    }
    return(variable)
}

#' Set an object's type based on a list of fields
#'
#' @inheritParams attr_from_field
type_from_field <- function(variable, fields, try_to_factorize) {
    type <- fields[["type"]]
    if (type == "string") {
        if (try_to_factorize == "always") {
            # convert all character to factor
            if (is.null(fields[["levels"]])) {
                try(variable <- as.factor(variable))
            } else {
                try(variable <- factor(variable, levels = fields[["levels"]]))
            }
        } else if (try_to_factorize == "conditional") {
            # convert character to factor if levels are present
            if (is.null(fields[["levels"]])) {
                try(variable <- as.character(variable))
            } else {
                try(variable <- factor(variable, levels = fields[["levels"]]))
            }
        } else {
            # do not convert character to factor
            try(variable <- as.character(variable))
        }
    } else if (type == "date") {
        try(variable <- as.Date(variable))
    } else if (type == "datetime") {
        try(variable <- as.POSIXct(variable))
    } else if (type == "boolean") {
        try(variable <- as.logical(variable))
    } else if (type == "number") {
        try(variable <- as.numeric(variable))
    } else {
        try(variable <- methods::as(variable, type))
    }
    return(variable)
}

