
# import libraries --------------------------------------------------------
library(flexdashboard)
library(tidyr)
library(dplyr)
library(stringr)
# library(forcats)
# library(tictoc)
library(rjson)


# import functions --------------------------------------------------------
setwd("/home/niklas/Coding/GITHUB_BIDS_Dashboard/")

source("functions.R")

# Environment variables ---------------------------------------------------

# Step 1: JSON Metadata Extraction ----------------------------------------


extract_json_metadata(working_dir)

# Step 2: Dashboards --------------------------------------------



