utils::globalVariables(c(
  "count", "total_row", "total_col", "value",
  "row_pct", "col_pct", "total"
))

# R/create_frequency_table_2vars.R
# meta --------------------------------------------------------------------
#' Create a two-variable frequency table in PDF
#'
#' Builds a cross-tabulation of `d_col` (rows) by `y_col` (columns) with
#' counts and, optionally, row/column percentages, and compiles it to a
#' standalone PDF via LaTeX.
#'
#' @param df A data frame.
#' @param d_col Character. Name of the grouping (row) variable.
#' @param y_col Character. Name of the target (column) variable.
#' @param output_name Character. Base name for output files. If `NULL`,
#'   derived from `d_col` and `y_col`.
#' @param output_dir Character. Directory where the `.tex` and `.pdf` are
#'   written. Defaults to the current working directory.
#' @param open_pdf Logical. Open the PDF after creation?
#' @param clean_temp Logical. Remove temporary LaTeX files?
#' @param pivot Logical. Swap `d_col` and `y_col` if `d_col` has fewer or
#'   equal unique values.
#' @param percentage_row Logical. Add row percentages?
#' @param percentage_col Logical. Add column percentages?
#'
#' @return Invisibly, the frequency table as a data frame.
#'
#' @examples
#' \dontrun{
#' data("starwars", package = "dplyr")
#' create_frequency_table_2vars(starwars, "species", "gender",
#'                              output_dir = tempdir())
#' }
#'
#' @importFrom rlang :=
#'
#' @importFrom dplyr select mutate group_by summarise ungroup
#' @importFrom dplyr left_join right_join bind_rows all_of n
#' @importFrom tidyr pivot_wider
#' @importFrom rlang sym
#' @importFrom stats na.omit
#' @importFrom utils capture.output tail
#' @importFrom xtable xtable
#--------------------------------------------------------------------------

create_frequency_table_2vars <- function(df,
                                         d_col,
                                         y_col,
                                         output_name = NULL,
                                         output_dir = getwd(),
                                         open_pdf = TRUE,
                                         clean_temp = TRUE,
                                         pivot = FALSE,
                                         percentage_row = TRUE,
                                         percentage_col = TRUE) {

  message("Creation of the frequency table: ", d_col, " x ", y_col)

  #---checks---
  if (!d_col %in% names(df)) {
    stop("No ", d_col, " in the dataframe :(")
  }

  if (!y_col %in% names(df)) {
    stop("No ", y_col, " in the dataframe :/")
  }

  # guard against a silent, meaningless table
  if (d_col == y_col) {
    stop("d_col and y_col must be different columns (got '", d_col, "' twice).")
  }

  #---create directory if necessary---
  if (!dir.exists(output_dir)) {
    message("Creating directory: ", output_dir)
    dir.create(output_dir,
               recursive = TRUE,
               showWarnings = FALSE)
  }

  #---prepare pivot---
  if (pivot) {
    n_group_unique <- length(unique(df[[d_col]]))
    n_category_unique <- length(unique(df[[y_col]]))

    if (n_group_unique <= n_category_unique) {
      message("pivot = TRUE. \nPivot table.")
      temp <- d_col
      d_col <- y_col
      y_col <- temp
    }
  }

  #---create output name by default---
  if (is.null(output_name)) {
    output_name <- paste0("freq_table_", d_col, "_x_", y_col)
  }

  #---create table---
  # warning of NAs
  df <- df |>
    select(!!sym(d_col),
           !!sym(y_col))

  na_count <- sum(is.na(df[[d_col]]) | is.na(df[[y_col]]))

  if (na_count > 0) {
    warning("You have ", na_count, " missing observations in your ", d_col, " and ", y_col, " variables. \nThese observations have been automatically removed from the table.")
  }

  # modify and clean data
  df <- df |>
    mutate(!!d_col := if(is.numeric(!!sym(d_col))) {
      as.integer(!!sym(d_col))
    } else {
      !!sym(d_col)
    }) |>
    na.omit()

  #---get all combinations of d_col and y_col---
  all_combinations <- expand.grid(
    unique(df[[d_col]]),
    unique(df[[y_col]])
  )
  names(all_combinations) <- c(d_col, y_col)

  #---create the base table with counts for all combinations---
  table_temp <- df |>
    group_by(!!sym(d_col), !!sym(y_col)) |>
    summarise(count = n(),
              .groups = 'drop') |>
    right_join(all_combinations, by = c(d_col, y_col)) |>
    mutate(count = ifelse(is.na(count), 0, count))

  #---helper to build a makecell value, blank if count is 0---
  make_cell <- function(count, row_pct = NA, col_pct = NA) {
    mapply(function(cnt, rp, cp) {
      if (cnt == 0) return("")
      s <- as.character(cnt)
      if(!is.na(rp)) s <- paste0(s, "\\\\ row: ", rp, "\\%")
      if(!is.na(cp)) s <- paste0(s, "\\\\ col: ", cp, "\\%")
      paste0("\\makecell{", s, "}")
    }, count, row_pct, col_pct)
  }

  grand_total <- sum(table_temp$count)

  #---add percentages based on options---
  if (percentage_row & percentage_col) {
    message("Create table with percentages by rows and columns.")

    table_temp <- table_temp |>
      group_by(!!sym(d_col)) |>
      mutate(
        total_row = sum(count),
        percentage_row = round(count / total_row * 100, 1)
      ) |>
      ungroup() |>
      group_by(!!sym(y_col)) |>
      mutate(
        total_col = sum(count),
        percentage_col = round(count / total_col * 100, 1)
      ) |>
      ungroup() |>
      mutate(
        value = make_cell(count, percentage_row, percentage_col)
      )

    } else if (percentage_row) {
      message("Create table with percentages by rows uniquely.")

      table_temp <- table_temp |>
        group_by(!!sym(d_col)) |>
        mutate(
          total_row = sum(count),
          percentage_row = round(count / total_row * 100, 1)
        ) |>
        ungroup() |>
        mutate(
          value = make_cell(count, row_pct = percentage_row)
        )

    } else if (percentage_col) {
    message("Create table with percentages by columns uniquely.")

    table_temp <- table_temp |>
      group_by(!!sym(y_col)) |>
      mutate(
        total_col = sum(count),
        percentage_col = round(count / total_col * 100, 1)
      ) |>
      ungroup() |>
      mutate(
        value = make_cell(count, col_pct = percentage_col)
      )

    } else {
    message("Create frequency table without percentages.")

    table_temp <- table_temp |>
      mutate(
        value = make_cell(count)
      )
    }

  #---pivot the main table---
  table_pivot <- table_temp |>
    select(!!sym(d_col), !!sym(y_col), value) |>
    pivot_wider(
      names_from = !!sym(y_col),
      values_from = value,
      values_fill = ""
    )

  #---create totals---
  table_total_col <- table_temp |>
    group_by(!!sym(y_col)) |>
    summarise(total_col = sum(count), .groups = 'drop') |>
    mutate(
      row_pct = if (percentage_row) round(total_col / grand_total * 100, 1) else NA,
      col_pct = if (percentage_col) 100 else NA,
      value = make_cell(total_col, row_pct, col_pct)
    ) |>
    select(!!sym(y_col), value) |>
    pivot_wider(
      names_from = !!sym(y_col),
      values_from = value,
      values_fill = ""
    ) |>
    mutate(!!sym(d_col) := "total")

  table_total_row <- table_temp |>
    group_by(!!sym(d_col)) |>
    summarise(total_row = sum(count), .groups = "drop") |>
    mutate(
      row_pct = if (percentage_row) 100 else NA,
      col_pct = if (percentage_col) round(total_row / grand_total * 100, 1) else NA,
      total = make_cell(total_row, row_pct, col_pct)
    ) |>
    select(!!sym(d_col), total)

  #---grand total (bottom-right corner)---
  grand_row_pct <- if (percentage_row) 100 else NA
  grand_col_pct <- if (percentage_col) 100 else NA
  grand_value <- make_cell(grand_total, grand_row_pct, grand_col_pct)

  table_total_col <- table_total_col |>
    mutate(
      total = grand_value
    )

  #---get all d_col values including "total"---
  all_d_cols <- c(unique(df[[d_col]]), "total")

  #---combine everything---
  table <- table_pivot |>
    left_join(table_total_row, by = d_col)

  table <- bind_rows(table, table_total_col)

  # reorder columns: d_col, then all y_cols, then "total" at the end
  y_cols <- setdiff(names(table), c(d_col, "total"))
  table <- table |>
    select(!!sym(d_col), all_of(y_cols), total)

  #---clean up column names for LaTeX
  table_original <- table

  #---final dimensions---
  n_rows <- nrow(table)
  n_cols <- ncol(table)

  #---create files---
  tex_filename      <- paste0(output_name, ".tex")
  wrapper_filename  <- paste0(output_name, "_wrapper.tex")
  final_pdf_name    <- paste0(output_name, ".pdf")

  tex_path          <- file.path(output_dir, tex_filename)
  wrapper_path      <- file.path(output_dir, wrapper_filename)
  pdf_path          <- file.path(output_dir, final_pdf_name)

  #---sanitize column names for LaTeX---
  clean_names <- function(x) gsub("_", "\\\\_", x)
  table_latex <- table
  names(table_latex) <- clean_names(names(table_latex))

  #---export to LaTeX---
  clean_caption <- paste0("Frequency table: ", gsub("_", " ", d_col))

  latex_table <- xtable(
    table_latex,
    caption = clean_caption,
    label = paste0("tab:", gsub("_", "", d_col), "_x_", gsub("_", "", y_col))
  )

  latex_lines <- capture.output(print(
    latex_table,
    type = "latex",
    include.rownames = FALSE,
    caption.placement = "top",
    booktabs = TRUE,
    hline.after = c(-1, 0, nrow(table)),
    sanitize.text.function = function(x) x
  ))

  # insert \midrule before the total
  total_line_index <- grep("total by columns",
                           latex_lines,
                           ignore.case = TRUE)

  if (length(total_line_index) > 0) {
    latex_lines <- append(latex_lines, "\\midrule", after = total_line_index[1] - 1)
    message("Added \\midrule before total row at line", total_line_index[1])
  } else {
    total_line_index <- grep("total",
                             latex_lines,
                             ignore.case = TRUE)

    if (length(total_line_index) > 0) {
      last_total <- tail(total_line_index, 1)
      latex_lines <- append(latex_lines, "\\midrule", after = last_total -1)
      message("Added \\midrule before total row (using 'total' fallback) at line ", last_total)
    }
  }

  # formattage
  tabular_start <- grep("\\\\begin\\{tabular\\}", latex_lines)
  tabular_end   <- grep("\\\\end\\{tabular\\}", latex_lines)

  latex_lines <- append(latex_lines, "\\tiny", after = tabular_start - 1)

  if (n_cols > 8) {
    latex_lines <- append(latex_lines, "\\resizebox{\\linewidth}{}{%", after = tabular_start)
  } else {
    latex_lines <- append(latex_lines, "\\resizebox{\\textwidth}{}{%", after = tabular_start)
  }

  latex_lines <- append(latex_lines, "}", after = tabular_end + 2)

  # write .tex file
  writeLines(latex_lines, tex_path)

  #---wrapper---
  is_landscape <- n_cols > 8

  wrapper <- sprintf(
    '\\documentclass[preview, border=2pt, 12pt%s]{standalone}
    \\usepackage{booktabs}
    \\usepackage{array}
    \\usepackage{makecell}
    \\usepackage{geometry}
    \\geometry{paperwidth=%sin, paperheight=%sin, margin=0.2in}
    \\begin{document}
    \\input{%s}
    \\end{document}',
    ifelse(is_landscape, ", landscape", ""),
    ifelse(is_landscape, 20, 12),
    ifelse(is_landscape, 12, 10),
    tex_filename
  )

  writeLines(wrapper, wrapper_path)

  #---compile to PDF---
  old_wd <- getwd() # remember current location
  on.exit(setwd(old_wd), add = TRUE) # schedule the "undo immediately"

  setwd(output_dir)

  compile_result <- system(paste("pdflatex -interaction=nonstopmode",
                                 shQuote(wrapper_filename)))

  #---rename and open the PDF---
  compiled_pdf_name <- sub("\\.tex$", ".pdf", wrapper_filename)
  compiled_pdf_path <- file.path(output_dir, compiled_pdf_name)

  if (file.exists(file.path(output_dir, compiled_pdf_name))) {
    file.rename(file.path(output_dir, compiled_pdf_name), pdf_path)

    if (open_pdf) {
      if (.Platform$OS.type == "windows") {
        shell.exec(pdf_path)
      } else {
        utils::browseURL(pdf_path)
      }
    message("PDF created: ", pdf_path)
    }
  } else {
    log_file <- file.path(output_dir, sub("\\.tex$", ".log", wrapper_filename))
    log_tail <- if (file.exists(log_file)) {
      tail(readLines(log_file, warn = FALSE), 15)
    } else {
      "(no .log file found either - pdflatex may not be on your PATH)"
    }
    warning("PDF not created. Last lines of the LaTeX log: \n",
            paste(log_tail, collapse = "\n"))
  }

  #---clean temporary files---
  if (clean_temp) {
    temp_files <- c(".aux", ".log", ".out", ".gz")

    for (ext in temp_files) {
      temp_file <- file.path(output_dir, sub("\\.tex$", ext, wrapper_filename))
      if (file.exists(temp_file)) {
        file.remove(temp_file)
      }
    }

    if (file.exists(wrapper_path)) {
      file.remove(wrapper_path)
    }

    message("Temporary files removed.")
  }

  message("Frequency table ", output_name, " created.")
  message("File .tex: ", tex_filename)
  message("File .pdf: ", final_pdf_name)

  invisible(table)
  message("\n \n Please cite as: Labarthe, S. (2026). Create Frequency Tables. An R function.")
  message("Also visit my website to discover more functions : https://github.com/SimonLabarthe")
}

# library(here)
# library(dplyr)
# library(tidyr)
# library(xtable)
#
# data("starwars")
#
# create_frequency_table_2vars(starwars, "hair_color", "eye_color", percentage_row = TRUE, percentage_col = TRUE)

