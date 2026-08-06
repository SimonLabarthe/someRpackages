# R/create_frequency_table_3vars.R
# meta --------------------------------------------------------------------
#' @Title - create_frequency_tables_3vars.R  
#' @Author - simon 
#' @Date - 2026-07-30
#' @Objective - create nested frequency table with 3 variables
#' @param df Dataframe
#' @param d1_col Grouping variable 1
#' @param d2_col Grouping variable 2
#' @param y_col Target variable
#' @param output_name Name for output files (optional, default uses d_col)
#' @param output_dir Directory for outputs (optional, default uses here())
#' @param open_pdf Open PDF after creation? (default: TRUE)
#' @param clean_temp Remove temporary LaTeX files? (default: TRUE)
#' @param pivot Invert the grouping and the target variable if the number of 
#'              unique values of the grouping variables is lower or equal to
#'              the number of unique values of the target variable (default : TRUE)
#' @return Invisible table (prints nothing)
#' @examples 
#' \dontrun{
#' library(dplyr)
#' library(here)
#' library(tidyr)
#' library(xtable)
#' 
#' data("starwars")
#' 
#' result <- create_frequency_table_3vars(
#'   df = starwars,
#'   d1_col = "sex",
#'   d2_col = "species",
#'   y_col = "gender"
#' )
#'   
#' # Sanity checks:
#' str(result)                     # confirms structure of the returned (invisible) table
#' getwd()                         # your working directory,
#' file.exists(here("..", "4_outputs", "tables", "freq_table_sex_x_species_x_gender.pdf")) # should be TRUE
#' }
#--------------------------------------------------------------------------

create_frequency_table_3vars <- function(df, 
                                         d1_col, 
                                         d2_col, 
                                         y_col, 
                                         output_name = NULL, 
                                         output_dir = here("../4_outputs", "tables"), 
                                         open_pdf = TRUE, 
                                         clean_temp = TRUE, 
                                         pivot = FALSE, 
                                         percentage_row = TRUE, 
                                         percentage_col = TRUE, 
                                         hline = FALSE) {
  
  message("Creating frequency table: ", d1_col, " x ", d2_col, " x ", y_col)
  
  #---checks---
  for (col in c(d1_col, 
                d2_col, 
                y_col)) {
    if (!col %in% names(df)) stop("No ", col, " in the dataframe :(")
  }
  
  # guard against a silent, meaningless table
  if (d1_col == d2_col | d1_col == y_col | d2_col == y_col) {
    warning("Grouping and output cols must be different columns")
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
    n_group_unique <- length(unique(df[[d1_col]]))
    n_category_unique <- length(unique(df[[y_col]]))
    
    if (n_group_unique <= n_category_unique) {
      message("pivot = TRUE. \nPivot table.")
      temp <- d1_col
      d1_col <- y_col
      y_col <- temp
    }
  }
  
  #---create output name by default---
  if (is.null(output_name)) {
    output_name <- paste0("freq_table_", d1_col, "_x_", d2_col, "_x_", y_col)
  }
  
  #---create table---
  # warning of NAs
  df <- df |> 
    select(!!sym(d1_col), 
           !!sym(d2_col), 
           !!sym(y_col))
  
  na_count <- sum(
    is.na(df[[d1_col]]) | 
      is.na(df[[d2_col]]) |
      is.na(df[[y_col]])
  )
  
  if (na_count > 0) {
    warning("You have ", na_count, "missing observations in your ", d1_col, ", ", d2_col, " and your ", y_col, " variables. \nThese observations have been automatically removed from the table.")
  }
  
  # modify and clean data
  df <- df |> 
    mutate(!!d1_col := if(is.numeric(!!sym(d1_col))) {
      as.integer(!!sym(d1_col))
    } else {
      !!sym(d1_col)
    }) |> 
    mutate(!!d2_col := if(is.numeric(!!sym(d2_col))) {
      as.integer(!!sym(d2_col))
    } else {
      !!sym(d2_col)
    }) |> 
    na.omit()
  
  #---get all combinations of d1_col, d2_col, and y_col---
  all_combinations <- expand.grid(
    unique(df[[d1_col]]), 
    unique(df[[d2_col]]), 
    unique(df[[y_col]])
  )
  names(all_combinations) <- c(d1_col, d2_col, y_col)
  
  #---create the base table with counts for all combinations---
  table_temp <- df |> 
    group_by(!!sym(d1_col), 
             !!sym(d2_col), 
             !!sym(y_col)) |> 
    summarise(count = n(), 
              .groups = 'drop') |> 
    right_join(all_combinations, by = c(d1_col, d2_col, y_col)) |> 
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
      group_by(!!sym(d1_col), !!sym(d2_col)) |> 
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
    message("Create frequency table with percentages by rows uniquely.")
    
    table_temp <- table_temp |> 
      group_by(!!sym(d1_col), !!sym(d2_col)) |> 
      mutate(
        total_row = sum(count), 
        percentage_row = round(count / total_row * 100, 1)
      ) |> 
      ungroup() |> 
      mutate(
        value = make_cell(count, percentage_row)
      )
    
  } else if (percentage_col) {
    message("Create frequency table with percentages by columns uniquely.")
    
    table_temp <- table_temp |> 
      group_by(!!sym(y_col)) |> 
      mutate(
        total_col = sum(count), 
        percentage_col = round(count / total_col * 100, 1)
      ) |> 
      ungroup() |> 
      mutate(
        value = make_cell(count, percentage_col)
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
    select(!!sym(d1_col), !!sym(d2_col), !!sym(y_col), value) |> 
    pivot_wider(
      names_from = !!sym(y_col), 
      values_from = value, 
      values_fill = ""
    ) |> 
    arrange(!!sym(d1_col), !!sym(d2_col))
  
  #---create totals---
  # Column totals (for each y_col category)
  table_total_col <- table_temp |> 
    group_by(!!sym(y_col)) |> 
    summarise(total_col = sum(count), 
              .groups = 'drop') |> 
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
    mutate(!!sym(d1_col) := "total", 
           !!sym(d2_col) := "")
  
  #---rows total (for each d1 x d2 combinations)---
  table_total_row <- table_temp |> 
    group_by(!!sym(d1_col), !!sym(d2_col)) |> 
    summarise(total_row = sum(count), 
              .groups = 'drop') |> 
    mutate(
      row_pct = if (percentage_row) 100 else NA, 
      col_pct = if (percentage_row) round(total_row / grand_total * 100, 1) else NA, 
      total = make_cell(total_row, row_pct, col_pct)
    ) |> 
    select(!!sym(d1_col), !!sym(d2_col), total)
  
  #---combine everything---
  table <- table_pivot |> 
    left_join(table_total_row, 
              by = c(d1_col, d2_col))
  
  #---get all y_col names---
  y_cols <- setdiff(names(table), c(d1_col, d2_col, "total"))
  
  #---create total row---
  table <- bind_rows(table, table_total_col)
  
  # add grand total for bottom-right corner (like in 2-vars version)
  grand_row_pct <- if (percentage_row) 100 else NA
  grand_col_pct <- if (percentage_col) 100 else NA
  grand_value <- make_cell(grand_total, grand_row_pct, grand_col_pct)
  
  # Add the grand total to the total row
  table_total_col <- table_total_col |> 
    mutate(
      total = grand_value
    )
  
  # Recreate the combined table with the grand total
  table <- table_pivot |> 
    left_join(table_total_row, by = c(d1_col, d2_col))
  
  table <- bind_rows(table, table_total_col)
  
  #--reorder---
  table <- table |> 
    select(!!sym(d1_col), !!sym(d2_col), all_of(y_cols), total)
  
  #---collapse repeated cells---
  # Store original d1 values before modification
  d1_original_values <- table[[d1_col]]
  
  d1_values <- unique(df[[d1_col]])
  n_d1_values <- length(d1_values)
  
  #---create a vector to store multirow labels---
  d1_labels <- character(nrow(table))
  
  rows_before_total <- which(table[[d1_col]] != "total")
  total_row_index <- which(table[[d1_col]] == "total")
  
  for (d1_val in d1_values) {
    
    # find rows for this d1 value
    rows <- which(table[[d1_col]] == d1_val)
    
    if(length(rows) > 0) {
      d1_labels[rows[1]] <-  paste0("\\multirow{", length(rows), "}{*}{", d1_val, "}")
      
      if (length(rows) > 1) {
        d1_labels[rows[-1]] <- ""
      }
    }
  }
  
  # handle the total row 
  if (length(total_row_index) > 0) {
    d1_labels[total_row_index] <- "total"
  }
  
  #---replace the d1_col with the multirow labels---
  table[[d1_col]] <- d1_labels
  
  #---store original table in environment---
  assign(output_name, table, envir = .GlobalEnv)
  
  #---final dimensions---
  n_rows <- nrow(table)
  n_cols <- ncol(table)
  
  #---file names---
  tex_filename <- paste0(output_name, ".tex")
  wrapper_filename <- paste0(output_name, "_wrapper.tex")
  final_pdf_name <- paste0(output_name, ".pdf")
  
  tex_path <- file.path(output_dir, tex_filename)
  wrapper_path <- file.path(output_dir, wrapper_filename)
  final_pdf_path <- file.path(output_dir, final_pdf_name)
  
  
  #---sanitize column names for LaTeX---
  clean_names <- function(x) gsub("_", "\\\\_", x)
  table_latex <- table
  names(table_latex) <- clean_names(names(table_latex))
  
  #---export to latex---
  clean_caption <- paste0("Nested frequency table: ", 
                          gsub("_", " ", d1_col), 
                          " x ", 
                          gsub("_", " ", d2_col),
                          " x ",
                          gsub("_", " ", y_col))
  
  latex_table <- xtable(
    table_latex, 
    caption = clean_caption, 
    label = paste0("tab:", d1_col, "_x_", d2_col, "_x_", y_col)
  )
  
  latex_lines <- capture.output(print(
    latex_table, 
    type = "latex", 
    include.rownames = FALSE, 
    caption.placement = "top", 
    booktabs = TRUE, 
    hline.after = c(-1, 0, nrow(table_latex)), 
    sanitize.text.function = function(x) x
  ))
  
  # insert midrule before total row 
  # Find the total row in the LaTeX output
  total_line_index <- grep("total", latex_lines, ignore.case = TRUE)
  
  if (length(total_line_index) > 0) {
    # Use the last occurrence of "total" (which should be the total row)
    last_total <- tail(total_line_index, 1)
    # Insert midrule before the total row
    latex_lines <- append(latex_lines, "\\midrule", after = last_total - 1)
    message("Added \\midrule before total row at line ", last_total)
  }
  
  #---insert hline between different d1_col values (optional)---
  if (hline) {
    # Use the original d1 values to identify group boundaries
    d1_original <- d1_original_values
    
    # Find positions where d1 changes (excluding total row)
    hline_positions <- c()
    current_d1 <- NULL
    
    # Only consider rows before the total row
    total_idx <- which(d1_original == "total")
    if (length(total_idx) > 0) {
      rows_to_check <- 1:(total_idx[1] - 1)
    } else {
      rows_to_check <- 1:length(d1_original)
    }
    
    if (length(rows_to_check) > 0) {
      current_d1 <- d1_original[rows_to_check[1]]
      
      for (i in 2:length(rows_to_check)) {
        row_idx <- rows_to_check[i]
        if (d1_original[row_idx] != current_d1) {
          # Insert hline before this row
          hline_positions <- c(hline_positions, row_idx - 1)
          current_d1 <- d1_original[row_idx]
        }
      }
    }
    
    # Insert hlines at the calculated positions
    if (length(hline_positions) > 0) {
      tabular_start <- grep("\\\\begin\\{tabular\\}", latex_lines)
      
      if (length(tabular_start) > 0) {
        header_midrule <- grep("\\\\midrule", latex_lines)[1]
        content_start <- header_midrule + 1  
        
        # Sort in descending order to avoid index shifting
        hline_positions <- sort(hline_positions, decreasing = TRUE)
        
        for (pos in hline_positions) {
          insert_after <- content_start + pos - 1
          if (insert_after <= length(latex_lines)) {
            latex_lines <- append(latex_lines, "\\midrule", after = insert_after)
          }
        }
        message("Added horizontal lines between different ", d1_col, " groups.")
      }
    }
  }
  
  #---formattage---
  tabular_start <- grep("\\\\begin\\{tabular\\}", latex_lines)
  tabular_end <- grep("\\\\end\\{tabular\\}", latex_lines)
  
  # insert \tiny before tabular
  if (length(tabular_start) > 0) {
    latex_lines <- append(latex_lines, "\\tiny", after = tabular_start - 1)
  }
  
  # wrap in resizebox
  if (length(tabular_start) > 0) {
    if (n_cols > 8) {
      latex_lines <- append(latex_lines, "\\resizebox{\\linewidth}{}{%", after = tabular_start)
    } else {
      latex_lines <- append(latex_lines, "\\resizebox{\\textwidth}{}{%", after = tabular_start)
    }
  } 
  
  if (length(tabular_end) > 0) {
    latex_lines <- append(latex_lines, "}", after = tabular_end + 2)
  }
  
  # write .tex file
  writeLines(latex_lines, tex_path)
  
  #---wrapper---
  is_landscape <- n_cols > 8
  
  wrapper <- sprintf(
    '\\documentclass[preview, border=2pt, 12pt%s]{standalone}
    \\usepackage{booktabs}
    \\usepackage{caption}
    \\usepackage{array}
    \\usepackage{makecell}
    \\usepackage{multirow}
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
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  setwd(output_dir)
  
  compile_result <- system(paste("pdflatex -interaction=nonstopmode", 
                                 shQuote(wrapper_filename)))
  
  #---rename and open the PDF---
  compiled_pdf_name <- sub("\\.tex$", ".pdf", wrapper_filename)
  compiled_pdf_path <- file.path(output_dir, compiled_pdf_name)
  
  if (file.exists(file.path(output_dir, compiled_pdf_name))) {
    file.rename(file.path(output_dir, compiled_pdf_name), final_pdf_path)
    
    if (open_pdf) {
      if(.Platform$OS.type == "windows") {
        shell.exec(final_pdf_path)
      } else {
        utils::browseURL(final_pdf_path)  
      }
      message("open_pdf = TRUE \nPDF created: ", final_pdf_path)
    }
  } else {
    log_file <- here(output_dir, sub("\\.tex$", ".log", wrapper_filename))
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
      temp_file <- here(output_dir, sub("\\.tex$", ext, wrapper_filename))
      
      if (file.exists(temp_file)) {
        file.remove(temp_file)
      }
    }
    
    if (file.exists(wrapper_path)) {
      file.remove(wrapper_path)
    }
    
    message("\nTemporary files removed. \n")
  }
  
  message("Nested frequency table ", output_name, " created.")
  message("File .tex: ", tex_path)
  message("File .pdf: ", final_pdf_path)
  
  invisible(table)
  message("\n\nPlease cite as: Labarthe, S. (2026). Create Frequency Tables. An R function.")
  message("Also visit my website to discover more functions : https://github.com/SimonLabarthe")
}






# library(here)
# library(dplyr)
# library(tidyr)
# library(xtable)
# 
# data("starwars")
# 
# create_frequency_table_3vars(starwars,
#                              "gender",
#                              "sex",
#                              "eye_color",
#                              percentage_row = TRUE,
#                              percentage_col = TRUE,
#                              hline = TRUE)






















