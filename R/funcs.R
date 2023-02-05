
clean_GCAM_query <- function(file, csv = TRUE, nskip = 1){
  if(csv == TRUE) file <- read.csv(file, skip = nskip)
  file %>%
    select(-matches("^X$")) %>%
    na.omit() %>%
    filter(scenario != "scenario") %>%
    return()
}

remove_date <- function(.data){
  .data %>%
    mutate(scenario = gsub(",date.*$", "", scenario)) %>%
    return()
}

gather_years <- function(.data){
  .data %>%
    gather(year, value, names(.)[grepl("[0-9]{4}", names(.))]) %>%
    mutate(year = as.integer(gsub("X", "", year))) %>%
    return()
}


read_file <- function(path = INPUT_DIR, pattern="*.csv$", 
                      assigngsub = ".csv", nskip = 0, proc = FALSE){
  if (proc == FALSE) {
    lapply(list.files(path, pattern), 
           function(file){assign(gsub(assigngsub,"",file),
                                 read.csv(file.path(path, file), 
                                          skip = nskip, 
                                          comment.char = "#"),
                                 envir = globalenv()) })
  }else if (proc == TRUE) {
    lapply(list.files(path, pattern), 
           function(file){assign(gsub(assigngsub,"",file),
                                 gather_years(
                                   remove_date(
                                     clean_GCAM_query(
                                       file.path(INPUT_DIR, file), nskip))),
                                 envir = globalenv()) })
  }

}


get_data_list <- function(all_data, data_list, strip_attributes = FALSE, environ = NULL) {
  # expecting a (potentially named) character vector for data_list
  assertthat::assert_that(is.character(data_list))
  data_list_names <- names(data_list)
  
  # strip_attributes must be logical and either be length 1 (in which case the same value
  # will be used for all calls to get_data) or match the length of data_list (values will be
  # matched up per index)
  assertthat::assert_that(is.logical(strip_attributes))
  assertthat::assert_that(length(strip_attributes) == 1 || length(strip_attributes) == length(data_list))
  if(length(strip_attributes) == 1) {
    strip_attributes = rep_len(strip_attributes, length(data_list))
  }
  
  # if no environment was explicitly given the default behavior is to load into the caller's
  # environment
  if(is.null(environ)) {
    environ = parent.frame()
  }
  
  # loop over each data_list, call get_data and assign the result to the same data name in
  # the given environment
  for(i in seq_along(data_list)) {
    curr_var_name <- data_list[i]
    # the variable name to assign for FILE is the "basename" of the file
    # i.e. `FILE = "common/GCAM_region_names"` will result in `GCAM_region_names` being set
    if(!is.null(data_list_names) && data_list_names[i] == "FILE") {
      # Note: strsplit returns a list (one per each str to be split) of character vector
      # (one for each token split out).  Given we are split one string at a time
      # we will just grab the first element of the list (`[[1]]`)
      data_name_split = strsplit(curr_var_name, "/")[[1]]
      # get the last element of the char vec to use as the var name
      curr_var_name = tail(data_name_split, n = 1)
    }
    # get the data
    data = get_data(all_data, data_list[i], strip_attributes[i])
    # assign it into the environment
    assign(curr_var_name, data, envir = environ)
  }
}

data.USER_MOD_POSTFIX <- "__0"
ATTR_TITLE <- "title"
ATTR_UNITS <- "units"
ATTR_COMMENTS <- "comments"
ATTR_PRECURSORS <- "precursors"
ATTR_LEGACY_NAME <- "legacy_name"
ATTR_REFERENCE <- "reference"
get_data <- function(all_data, name, strip_attributes = FALSE) {
  assertthat::assert_that(is_data_list(all_data))
  
  names(all_data) <- gsub(data.USER_MOD_POSTFIX, '', names(all_data))
  if(is.null(all_data[[name]])) {
    stop("Data system: couldn't find ", name)
  }
  
  # If a chunk's output is missing, it returns a tibble with all NA values
  # In this case we don't want to copy it to main data list, so that subsequent
  # chunks an easily check for its status via is.null()
  if(nrow(all_data[[name]]) > 0 && all(is.na(all_data[[name]]))) {
    return(NULL)
  }
  
  # If strip_attributes == TRUE, remove all attributes.
  # As of dplyr 1.0.0, these can no longer be easily overwritten, so we remove them
  if(strip_attributes) {
    attr(all_data[[name]], ATTR_TITLE) <- NULL
    attr(all_data[[name]], ATTR_UNITS) <- NULL
    attr(all_data[[name]], ATTR_COMMENTS) <- NULL
    attr(all_data[[name]], ATTR_PRECURSORS) <- NULL
    attr(all_data[[name]], ATTR_LEGACY_NAME) <- NULL
    attr(all_data[[name]], ATTR_REFERENCE) <- NULL
    all_data[[name]]
  } else {
    all_data[[name]]
  }
}

is_data_list <- function(data_list) {
  is.list(data_list)
}


