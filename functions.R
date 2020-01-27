
# find jsons --------------------------------------------------------------

index_jsons <- function(path) {
  print("Indexing JSON files")
  start <- start_time()
  json <- list.files(
    path = paste0(path),
    pattern = ".json",
    full.names = FALSE,
    recursive = TRUE,
    include.dirs = TRUE
  ) 
  measure_time(1, 1, start)
  return(json)
}


# Time measurement --------------------------------------------------------
list_items <- function(i, json, string) {
  cat("\014")
  print(string)
  print(paste(
    i,
    " / ",
    length(json),
    " total. (",
    round(i / length(json) * 100, 1),
    "%)"))
  print(paste(
    "json:",
    json[i]
  ))
}

start_time <- function() {
  Sys.time()
}
measure_time <- function(i, df, start) {
  end <- Sys.time()
  time_difference <- difftime(end, start, unit = "mins") %>% round(2)
  print(paste(
    "Time since start: ",
    time_difference,
    " min.  ETA: ",
    (difftime(end, start, unit = "mins")/i*length(df) - time_difference) %>% round(2),
    " min. remaining."
  ))
}




# Extraction of JSON metadata ---------------------------------------------
get_json_headers <- function(json) {
  setwd(working_dir)
  start <- start_time()
  for (i in 1:length(json)) {
    list_items(i, json, "Extraction of Headers - appending to one structure")
    measure_time(i, json, start)
    
    if (i == 1) {
      mri_properties_new <- names(fromJSON(file = json[i]))
      mri_properties <- mri_properties_new
    }
    if (i > 1) {
      mri_properties_new <- names(fromJSON(file = json[i]))
      mri_properties <- union(mri_properties, mri_properties_new)
      # print(setdiff(mri_properties, mri_properties_new))
    }
  }
  names = mri_properties %>% sort()
  empty_df <- data.frame()
  for (k in names)
    empty_df[[k]] <- as.character()
  print("Success!")
  return(empty_df)
}

# Read metadata from files ------------------------------------------------
read_json_headers <- function(json, empty_df) {
  setwd(working_dir)
  if (file.exists("json_files.csv") == 1) {
    file.remove("json_files.csv")
    print("json_files.csv removed!")
  }
  
  start <- start_time()
  for (i in 1:length(json)) {
    list_items(i, json, "Reading metadata into structure")
    measure_time(i, json, start)
    result_new <- fromJSON(file = json[i], simplify = TRUE) %>% 
      lapply(paste, collapse = ", ") %>% 
      bind_rows() %>%
      mutate(Path = json[i])
    result_new <- merge(empty_df, result_new, all = TRUE, sort = F)
    result_new <- result_new[sort(names(result_new))]
    
    # result_new_1 <-
    #   result_new[, order(colnames(empty_df), decreasing = TRUE)]
    
    if (file.exists("json_files.csv") == 0) {
      write.table(
        result_new,
        "json_files.csv",
        sep = ",",
        dec = ".",
        qmethod = "double",
        row.names = FALSE
      )
    } else {
      # Here data gets only appended to csv
      write.table(
        result_new,
        "json_files.csv",
        sep = ",",
        dec = ".",
        qmethod = "double",
        row.names = FALSE,
        append = TRUE,
        col.names = FALSE
      )
    }
  }
  print("Done!")
}

extract_metadata <- function(df, number) {
  df %>% filter(level == number) %>% select_if(~!all(is.na(.)))
}

