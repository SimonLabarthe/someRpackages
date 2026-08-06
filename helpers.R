# helpers.R
# Small helper functions used by all scripts

check_dependencies <- function() {
  required <- c("dplyr", "tidyr", "xtable", "here")
  missing <- required[!required %in% installed.packages()[, "Package"]]
  
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing)
  }
  
  invisible(TRUE)
}

check_latex <- function() {
  if (Sys.which("pdflatex") == "") {
    message("pdflatex not found. For PDF output, install LaTeX with:")
    message("install.packages('tinytex')")
    message("tinytex::install_tinytex()")
    return(FALSE)
  }
  return(TRUE)
}
