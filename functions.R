
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
get_json_headers <- function(json, working_dir) {
  setwd(working_dir)
  start <- start_time()
  
  mri_properties <- vector()
  str(mri_properties)
  for (i in 1:length(json)) {
    list_items(i, json, "Extraction of Headers - appending to one structure")
    measure_time(i, json, start)
    # Reading json headers
    mri_properties_new <- names(rjson::fromJSON(file = json[i]))
    mri_properties <- union(mri_properties, mri_properties_new)
  }
  # Building df
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
  if (file.exists("../json_files.tsv") == 1) {
    file.remove("../json_files.tsv")
    print("../json_files.csv removed!")
  }
  
  start <- start_time()
  for (i in 1:length(json)) {
    list_items(i, json, "Reading metadata into structure")
    measure_time(i, json, start)
    result_new <- rjson::fromJSON(file = json[i], simplify = TRUE) %>% 
      lapply(paste, collapse = ", ") %>% 
      bind_rows() %>%
      mutate(Path = json[i])
    result_new <- merge(empty_df, result_new, all = TRUE, sort = F)
    result_new <- result_new[sort(names(result_new))]
    
    # result_new_1 <-
    #   result_new[, order(colnames(empty_df), decreasing = TRUE)]
    
    if (file.exists("../json_files.tsv") == 0) {
      write.table(
        result_new,
        "../json_files.tsv",
        sep = "\t",
        dec = ".",
        qmethod = "double",
        row.names = FALSE
      )
    } else {
      # Here data gets only appended to csv
      write.table(
        result_new,
        "../json_files.tsv",
        sep = "\t",
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

extract_json_metadata <- function(working_dir) {
  json_files <- tibble(files = index_jsons(working_dir)) 
  metadata_empty_df <- get_json_headers(json_files$files, working_dir)
  metadata_df <- read_json_headers(json_files$files, metadata_empty_df)
  
}

read_metadata <- function() {
  metadata_df <- readr::read_tsv("../json_files.tsv") %>% 
    mutate(level = str_count(Path, "/")) %>% 
    separate(Path, into = c("subject", "session", "type", "filename"), sep = "/") %>%
    mutate(sequence = str_remove_all(filename, pattern = session) %>% 
             str_remove(subject) %>% 
             str_remove("__") %>% 
             str_remove(".json") %>%
             str_remove("-[:graph:]{1}$")) 
  return(metadata_df)
}


datatable_setting <- function(df) {
  DT::datatable(
    df,
    extensions = c('Buttons', 'Scroller'),
    options = list(
      search = list(regex = TRUE),
      searchHighlight = TRUE,
      pageLength = 25,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'print'),
      deferRender = TRUE,
      scrollY = 200,
      scroller = TRUE
    )
  )
}

show_settings <- function(sequence_id) {
  metadata_level_3 %>%
    filter(str_detect(sequence, sequence_id) == 1) %>%
    select(-filename,-subject,-session,-type,-level) %>%
    select(
      -AcquisitionNumber,
      -ImageOrientationPatientDICOM,
      -ImageType,
      -ProcedureStepDescription,
      -AccessionNumber,
      -StudyID,
      -StudyInstanceUID,
      -SeriesNumber,
      -SeriesInstanceUID
    ) %>%
    select(
      -AcquisitionDateTime,
      -AcquisitionTime,
      -PatientBirthDate,
      -PatientID,
      -PatientSex,
      -PatientName,
      -PatientWeight,
      -PhilipsRescaleSlope
    ) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    group_by_all() %>%
    count()
}


show_settings_external <- function(sequence_id) {
  metadata_level_3 %>%
    filter(str_detect(sequence, sequence_id) == 1) %>%
    select(-filename,
           -subject,
           -session,
           -type,
           -level) %>%
    select(
      -AcquisitionNumber,
      -ImageOrientationPatientDICOM,
      -ImageType,
      -ProcedureStepDescription,
      -AccessionNumber,
      -StudyID,
      -StudyInstanceUID,
      -SeriesNumber,
      -SeriesInstanceUID
    ) %>%
    select(
      -AcquisitionDateTime,
      -AcquisitionTime,

      -PatientID,

      -PatientName,

      -PhilipsRescaleSlope
    ) %>%
    mutate_if(is.numeric, round, digits = 2) %>%
    group_by_all() %>%
    count()
}
