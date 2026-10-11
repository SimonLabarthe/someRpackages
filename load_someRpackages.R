load_someRpackages <- function() {
  base_url <- "https://raw.githubusercontent.com/SimonLabarthe/someRpackages/main/R"

  files <- c(
    "utils.R",
    "create_descriptive_table.R",
    "create_frequency_table_2vars.R",
    "create_frequency_table_3vars.R"
  )

  for (f in files) {
    source(paste0(base_url, "/", f))
  }

  message("someRpackages functions loaded!")
  message("Have fun :)")
}
