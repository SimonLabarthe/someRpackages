# R/create_descriptive_table.R
# meta --------------------------------------------------------------------
#' @Title - create_descriptive_table.R  
#' @Author - Simon Labarthe 
#' @Date - 2026-08-06
#' @Objective - create descriptive statistics table in PDF
#' @param df Dataframe
#' @param output_name Name for output files (optional)
#' @param output_dir Directory for outputs (optional, default uses here())
#' @param open_pdf Open PDF after creation? (default: TRUE)
#' @param clean_temp Remove temporary LaTeX files? (default: TRUE)
#' @param mean Include mean for numeric variables? (default: TRUE)
#' @param median Include median for numeric variables? (default: TRUE)
#' @param sd Include standard deviation for numeric variables? (default: TRUE)
#' @param min Include minimum for numeric variables? (default: TRUE)
#' @param max Include maximum for numeric variables? (default: TRUE)
#' @param firstquartile Include first quartile (Q1) for numeric variables? (default: FALSE)
#' @param thirdquartile Include third quartile (Q3) for numeric variables? (default: FALSE)
#' @param mode Include mode for categorical variables? (default: TRUE)
#' @param arrange Reorder the variables by category and names (alphabetical order, default: TRUE)
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
#' result <- create_descriptive_table(
#'   df = starwars,
#'   output_dir = here(),
#'   mean = TRUE,
#'   median = TRUE,
#'   sd = TRUE,
#'   min = TRUE,
#'   max = TRUE,
#'   firstquartile = TRUE,
#'   thirdquartile = TRUE,
#'   mode = TRUE, 
#'   arrange = TRUE
#'   )
#' }
#--------------------------------------------------------------------------

create_descriptive_table <- function(df, 
                                     output_name = NULL, 
                                     output_dir = here(), 
                                     open_pdf = TRUE,
                                     clean_temp = TRUE, 
                                     mean = TRUE, 
                                     median = TRUE, 
                                     sd = TRUE, 
                                     min = TRUE, 
                                     max = TRUE, 
                                     firstquartile = FALSE, 
                                     thirdquartile = FALSE,
                                     mode = TRUE, 
                                     arrange = FALSE) {
  
  message("Creation of the descriptive statistics table")
  
  #---checks---
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  # Check if at least one statistic is requested
  stat_checks <- c(mean, median, sd, min, max, firstquartile, thirdquartile, mode)
  if (!any(stat_checks)) {
    stop("At least one statistic must be selected")
  }
  
  #---create directory if necessary---
  if (!dir.exists(output_dir)) {
    message("Creating directory: ", output_dir)
    dir.create(output_dir, 
               recursive = TRUE, 
               showWarnings = FALSE)
  }
  
  #---create output name by default---
  if (is.null(output_name)) {
    output_name <- paste0("descriptive_table_", deparse(substitute(df)))
  }
  
  #---Calculate statistics for each column---
  results_list <- list()
  
  for (col_name in names(df)) {
    col_data <- df[[col_name]]
    
    # Remove NAs for this column
    col_clean <- col_data[!is.na(col_data)]
    
    if (length(col_clean) == 0) {
      warning("Column '", col_name, "' has no valid observations after removing NAs. Skipping.")
      next
    }
    
    # Determine variable type
    is_numeric <- is.numeric(col_clean)
    is_categorical <- is.factor(col_clean) || is.character(col_clean) || is.logical(col_clean)
    
    # Start building row with all possible columns initialized to NA
    row_stats <- list(
      Variable = col_name,
      Type = if(is_numeric) "Numeric" else if(is_categorical) "Categorical" else "Other",
      n = length(col_clean),
      Mode = NA,
      Mean = NA,
      Median = NA,
      SD = NA,
      Min = NA,
      Max = NA,
      Q1 = NA,
      Q3 = NA
    )
    
    # For numeric variables
    if (is_numeric) {
      if (mean) row_stats$Mean <- round(mean(col_clean), 2)
      if (median) row_stats$Median <- round(median(col_clean), 2)
      if (sd) row_stats$SD <- round(sd(col_clean), 2)
      if (min) row_stats$Min <- round(min(col_clean), 2)
      if (max) row_stats$Max <- round(max(col_clean), 2)
      if (firstquartile) row_stats$Q1 <- round(quantile(col_clean, 0.25), 2)
      if (thirdquartile) row_stats$Q3 <- round(quantile(col_clean, 0.75), 2)
      
      # Mode for numeric (if requested)
      if (mode) {
        # For numeric, mode is the most frequent value
        freq_table <- table(col_clean)
        mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
        row_stats$Mode <- mode_value
      }
      
    } else if (is_categorical) {
      # Mode for categorical (most frequent category)
      if (mode) {
        freq_table <- table(col_clean)
        mode_value <- names(freq_table)[which.max(freq_table)]
        mode_count <- max(freq_table)
        mode_percent <- round(mode_count / length(col_clean) * 100, 1)
        row_stats$Mode <- paste0(mode_value, " (", mode_count, "/", mode_percent, "%)")
      }
      
    } else {
      # Other types (e.g., Date, complex) - skip
      warning("Column '", col_name, "' has unsupported type. Skipping.")
      next
    }
    
    results_list[[col_name]] <- row_stats
  }
  
  # Check if we have any results
  if (length(results_list) == 0) {
    stop("No valid columns found to calculate statistics")
  }
  
  # Convert to data frame - ensure all rows have same structure
  table <- do.call(rbind, lapply(results_list, function(x) {
    # Convert list to data frame with consistent column order
    df_row <- data.frame(
      Variable = x$Variable,
      Type = x$Type,
      n = x$n,
      Mode = ifelse(is.null(x$Mode), NA, x$Mode),
      Mean = ifelse(is.null(x$Mean), NA, x$Mean),
      Median = ifelse(is.null(x$Median), NA, x$Median),
      SD = ifelse(is.null(x$SD), NA, x$SD),
      Min = ifelse(is.null(x$Min), NA, x$Min),
      Max = ifelse(is.null(x$Max), NA, x$Max),
      Q1 = ifelse(is.null(x$Q1), NA, x$Q1),
      Q3 = ifelse(is.null(x$Q3), NA, x$Q3),
      stringsAsFactors = FALSE
    )
    return(df_row)
  }))
  
  # Reset row names
  rownames(table) <- NULL
  
  # Reorder columns: Variable, Type, n, then statistics in requested order
  stat_cols <- c("n")
  if (mode) stat_cols <- c(stat_cols, "Mode")
  if (mean) stat_cols <- c(stat_cols, "Mean")
  if (median) stat_cols <- c(stat_cols, "Median")
  if (sd) stat_cols <- c(stat_cols, "SD")
  if (min) stat_cols <- c(stat_cols, "Min")
  if (max) stat_cols <- c(stat_cols, "Max")
  if (firstquartile) stat_cols <- c(stat_cols, "Q1")
  if (thirdquartile) stat_cols <- c(stat_cols, "Q3")
  
  # Only keep columns that exist
  stat_cols <- stat_cols[stat_cols %in% names(table)]
  table <- table[, c("Variable", "Type", stat_cols)]
  
  #---arrange if requested---
  if (arrange) {
    table <- table |> 
      arrange(desc(Type == "Numeric"), Variable)
  }
  
  # Replace NA values with empty strings for better display
  table[is.na(table)] <- ""
  
  # Clean variable names for LaTeX (replace underscores with spaces)
  table$Variable <- gsub("_", " ", table$Variable)
  
  # Assign to global environment
  assign(output_name, table, envir = .GlobalEnv)
  
  #---export to LaTeX and PDF---
  # Get dimensions
  n_rows <- nrow(table)
  n_cols <- ncol(table)
  
  # Create filenames
  tex_filename <- paste0(output_name, ".tex")
  wrapper_filename <- paste0(output_name, "_wrapper.tex")
  final_pdf_name <- paste0(output_name, ".pdf")
  
  tex_path <- file.path(output_dir, tex_filename)
  wrapper_path <- file.path(output_dir, wrapper_filename)
  pdf_path <- file.path(output_dir, final_pdf_name)
  
  # Create caption
  clean_caption <- paste0("Descriptive statistics of dataset")
  
  # Custom sanitization function for LaTeX
  sanitize_for_latex <- function(x) {
    if (is.character(x)) {
      # Escape special LaTeX characters
      x <- gsub("\\\\", "\\\\textbackslash{}", x)
      x <- gsub("\\$", "\\\\$", x)
      x <- gsub("\\%", "\\\\%", x)
      x <- gsub("\\&", "\\\\&", x)
      x <- gsub("\\#", "\\\\#", x)
      x <- gsub("\\_", "\\\\_", x)
      x <- gsub("\\{", "\\\\{", x)
      x <- gsub("\\}", "\\\\}", x)
      x <- gsub("\\~", "\\\\textasciitilde{}", x)
      x <- gsub("\\^", "\\\\textasciicircum{}", x)
      return(x)
    }
    return(x)
  }
  
  # Export to LaTeX with proper sanitization
  latex_table <- xtable(
    table, 
    caption = clean_caption,
    label = paste0("tab:", gsub("_", "", output_name))
  )
  
  latex_lines <- capture.output(print(
    latex_table, 
    type = "latex", 
    include.rownames = FALSE, 
    caption.placement = "top", 
    booktabs = TRUE, 
    hline.after = c(-1, 0, nrow(table)),
    sanitize.text.function = sanitize_for_latex,
    sanitize.colnames.function = sanitize_for_latex
  ))
  
  # Remove any package declarations from the content
  latex_lines <- latex_lines[!grepl("^\\\\usepackage", latex_lines)]
  
  # Add resize and formatting
  tabular_start <- grep("\\\\begin\\{tabular\\}", latex_lines)
  tabular_end <- grep("\\\\end\\{tabular\\}", latex_lines)
  
  if (length(tabular_start) > 0 && length(tabular_end) > 0) {
    resize_width <- if (n_cols > 8) "\\linewidth" else "\\textwidth"
    
    # Prepend \tiny + \resizebox opening directly onto the \begin{tabular} line
    latex_lines[tabular_start] <- paste0(
      "\\tiny\n\\resizebox{", resize_width, "}{!}{%\n",
      latex_lines[tabular_start]
    )
    
    # Append closing brace directly onto the \end{tabular} line
    latex_lines[tabular_end] <- paste0(latex_lines[tabular_end], "\n}")
  }
  
  # Write .tex file
  writeLines(latex_lines, tex_path)
  
  # Create wrapper with all necessary packages
  is_landscape <- n_cols > 8
  
  wrapper <- sprintf(
    '\\documentclass[preview, border=2pt, 12pt%s]{standalone}
    \\usepackage{booktabs}
    \\usepackage{array}
    \\usepackage{makecell}
    \\usepackage{geometry}
    \\usepackage{graphicx}
    \\usepackage{caption}
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
  
  # Compile to PDF
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  setwd(output_dir)
  
  # Run pdflatex twice to resolve references
  compile_result <- system(paste("pdflatex -interaction=nonstopmode", 
                                 shQuote(wrapper_filename)))
  
  if (compile_result == 0) {
    # Run second time to resolve references
    system(paste("pdflatex -interaction=nonstopmode", 
                 shQuote(wrapper_filename)))
  }
  
  # Rename and open PDF
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
      tail(readLines(log_file, warn = FALSE), 20)
    } else {
      "(no .log file found - pdflatex may not be on your PATH)"
    }
    warning("PDF not created. Last lines of the LaTeX log: \n", 
            paste(log_tail, collapse = "\n"))
  }
  
  # Clean temporary files
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
  
  message("Descriptive table ", output_name, " created.")
  message("File .tex: ", tex_filename)
  message("File .pdf: ", final_pdf_name)
  
  invisible(table)
  message("\nPlease cite as: Labarthe, S. (2026). Create Descriptive Statistics Tables. An R function.")
  message("Visit my website: https://github.com/SimonLabarthe")
}

