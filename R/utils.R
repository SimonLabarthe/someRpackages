#' Check that required packages are installed
#'
#' Internal helper. Verifies that the packages listed in `required` are
#' available and installs any that are missing.
#'
#' @param required Character vector of package names to check.
#'
#' @return Invisibly `TRUE`.
#'
#' @noRd
check_dependencies <- function(required = c("dplyr", "tidyr", "xtable", "here")) {
  missing <- required[!required %in% utils::installed.packages()[, "Package"]]

  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    utils::install.packages(missing)
  }

  invisible(TRUE)
}

#' Check whether a LaTeX engine is available
#'
#' Internal helper. Tests whether `pdflatex` is on the `PATH` and prints
#' installation instructions if not.
#'
#' @return `TRUE` if `pdflatex` is found, `FALSE` otherwise.
#'
#' @noRd
check_latex <- function() {
  if (Sys.which("pdflatex") == "") {
    message("pdflatex not found. For PDF output, install LaTeX with:")
    message("install.packages('tinytex')")
    message("tinytex::install_tinytex()")
    return(FALSE)
  }
  return(TRUE)
}
