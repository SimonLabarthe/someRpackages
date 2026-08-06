# load_all.R
# One script to load all functions from myRpackages

load_myRpackages <- function() {
  base_url <- "https://raw.githubusercontent.com/SimonLabarthe/myRpackages/main"
  
  # Source helper
  source(paste0(base_url, "/helpers.R"))
  
  # Source all functions
  source(paste0(base_url, "/R/create_descriptive_table.R"))
  source(paste0(base_url, "/R/create_frequency_table_2vars.R"))
  source(paste0(base_url, "/R/create_frequency_table_3vars.R"))
  source(paste0(base_url, "/R/categorise_api.R"))
  
  message("All myRpackages functions loaded!")
  message("Have fun :)")
}