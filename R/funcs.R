
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

