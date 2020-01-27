
# import libraries --------------------------------------------------------
library(flexdashboard)
library(tidyr)
library(dplyr)
library(stringr)
library(forcats)
library(tictoc)
library(jsonlite)
library(DT)
library(highcharter)


# import functions --------------------------------------------------------
setwd("/home/niklas/Coding/GITHUB_BIDS_Dashboard/")

source("functions.R")

# Environment variables ---------------------------------------------------
working_dir <- ("/mnt/TB8Drive/Bidirect_Neuroimaging/BIDS/base_protocol/sourcedata/") 




# Step 1: JSON Metadata Extraction ----------------------------------------

json_files <- tibble(files = index_jsons(working_dir)) 
metadata_empty_df <- get_json_headers(json_files$files)
metadata_df <- read_json_headers(json_files$files, metadata_empty_df)

# Step 2: Tidying up data -----------------------------------------
json_metadata <- readr::read_csv("json_files.csv") %>% 
  mutate(level = str_count(Path, "/"))

json_metadata %>% count(level)

json_metadata_dataset <- json_metadata %>% filter(!is.na(Authors)) %>% extract_metadata(0)
json_metadata_level_0 <- json_metadata %>% filter(is.na(Authors)) %>% extract_metadata(0)
json_metadata_level_1 <- json_metadata %>% filter(is.na(Authors)) %>% extract_metadata(1)
json_metadata_level_2 <- json_metadata %>% filter(is.na(Authors)) %>% extract_metadata(2)
json_metadata_level_3 <- json_metadata %>% filter(is.na(Authors)) %>% extract_metadata(3)

# Step 3: Dashboards --------------------------------------------



