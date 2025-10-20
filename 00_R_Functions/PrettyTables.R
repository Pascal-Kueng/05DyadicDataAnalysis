# Main Functions in this file (2):

###################################
# 1. print_df
###################################
# Description:
# This function generates a styled HTML table from a data frame using the kableExtra package. It supports row packing, which groups consecutive rows under a common header. The resulting table is formatted and styled with options for width, scrolling, and packing.
#
# Arguments:
# - df: A data frame to be converted into an HTML table.
# - width: Column width of the table (default is "auto").
# - rows_to_pack: A named list of vectors specifying which rows to group under a common header.
# - scroll_height: Height of the scroll box for the table (default is '500px').
# - scroll_width: Width of the scroll box for the table (default is '100%').
#
# Example:
# print_df(df, width = "200px", rows_to_pack = list("Group1" = c(2, 5)), scroll_height = '400px', scroll_width = '80%')


###################################
# 2. export_xlsx
###################################
# Description:
# This function exports an HTML table to an Excel file. It supports merging identical cells, applying conditional formatting, and customizing column widths. The function also handles row packing for specific rows based on user input.
#
# Arguments:
# - html_table: An HTML table as a character string to be exported.
# - file: Path to the output Excel file.
# - merge_option: Specifies whether to merge cells in the header, data, or both.
# - simplify_2nd_row: Logical flag to simplify the second row of the table (default is FALSE).
# - colwidths: A vector specifying the width of each column in the output Excel file.
# - verbose: Logical flag to print progress messages (default is FALSE).
# - rows_to_pack: A named list of vectors specifying which rows to pack under a common header.
#
# Example:
# export_xlsx(html_table, file = "output.xlsx", merge_option = "both", simplify_2nd_row = TRUE, colwidths = c(20, 30, 25), verbose = TRUE, rows_to_pack = list("Header1" = c(2, 4)))




# Helper function to validate rows_to_pack input
validate_packing_input <- function(rows_to_pack, n_rows) {
  if (!is.null(rows_to_pack)) {
    if (!is.list(rows_to_pack) || !all(sapply(rows_to_pack, is.vector)) || 
        !all(sapply(rows_to_pack, function(x) length(x) == 2)) || 
        !all(sapply(rows_to_pack, function(x) all(sapply(x, is.numeric))))) {
      stop("Error: rows_to_pack must be a named list of vectors, each containing exactly two integers.")
    }
    
    if (!all(sapply(rows_to_pack, function(x) x[2] >= x[1]))) {
      stop("Error: In each vector, the second number must be equal to or larger than the first number.")
    }
    
    previous_end <- -Inf
    for (i in seq_along(rows_to_pack)) {
      if (rows_to_pack[[i]][1] <= previous_end) {
        stop(paste0("Error: The starting integer of vector '", names(rows_to_pack)[i], "' must be larger than the ending integer of the previous vector."))
      }
      previous_end <- rows_to_pack[[i]][2]
    }
    
    if (!all(sapply(rows_to_pack, function(x) all(x >= 1 & x <= n_rows)))) {
      stop("Error: Values in rows_to_pack must be within the row range of the data frame.")
    }
  }
}

# Helper function to handle row packing
packing <- function(kable_obj, rows_to_pack) {
  if (is.null(rows_to_pack)) return(kable_obj)
  
  for (i in seq_along(rows_to_pack)) {
    title <- names(rows_to_pack)[[i]]
    values <- rows_to_pack[[i]]
    kable_obj <- pack_rows(kable_obj, title, values[1], values[2])
  }
  return(kable_obj)
}

# Helper function to check and load required packages
check_and_load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# Main function for kable printing
print_df <- function(df, width = "auto", rows_to_pack = NULL, scroll_height = '500px', scroll_width = '100%') {
  # Check and load required packages
  required_packages <- c("knitr", "kableExtra", "dplyr")
  check_and_load_packages(required_packages)
  
  # Validate the input
  validate_packing_input(rows_to_pack, nrow(df))
  
  # Create kable object
  kable_obj <- kable(df) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "responsive"),
      full_width = F,
      position = "left",
      fixed_thead = T
    ) %>%
    column_spec(2:ncol(df), width = width) %>%
    row_spec(1:nrow(df), extra_css = "white-space: nowrap;") %>%
    scroll_box(width = scroll_width, height = scroll_height)
  
  # Apply packing if needed
  kable_obj <- packing(kable_obj, rows_to_pack)
  
  # Return the final kable object
  return(kable_obj)
}

# Helper function to validate merge_option
validate_merge_option <- function(merge_option) {
  if (!merge_option %in% c("header", "data", "both")) {
    stop("Invalid merge_option. Choose 'header', 'data', or 'both'.")
  }
}

# Helper function to validate colwidths
validate_colwidths <- function(colwidths, n_cols) {
  if (!is.null(colwidths) && length(colwidths) != n_cols) {
    stop("Colwidths vector must be the same size as the number of columns.")
  }
}

# Helper function to validate and format the file path
validate_file_path <- function(file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
}

# Helper function to merge identical cells in a row
merge_identical_cells <- function(wb, sheet, final, row_index, is_header = FALSE) {
  row_data <- if (is_header) {
    names(final)
  } else {
    unlist(final[row_index - 1, ])
  }
  
  col <- 1
  while (col <= length(row_data)) {
    start_col <- col
    
    # Continue as long as adjacent cells are identical AND not empty
    while (
      col < length(row_data) && 
      !is.na(row_data[col]) && 
      row_data[col] != "" && 
      identical(row_data[col], row_data[col + 1])
    ) {
      col <- col + 1
    }
    
    # Merge only if we found a span of identical cells and the first one isn't empty
    if (
      col > start_col && 
      !is.na(row_data[start_col]) && 
      row_data[start_col] != ""
    ) {
      mergeCells(wb, sheet, cols = start_col:col, rows = row_index)
      style <- createStyle(halign = "center")
      if (is_header) {
        style <- createStyle(halign = "center", textDecoration = "bold")
      }
      addStyle(
        wb,
        sheet  = sheet,
        style  = style,
        rows   = row_index,
        cols   = start_col:col,
        gridExpand = TRUE,
        stack      = TRUE
      )
    }
    
    col <- col + 1
  }
}


# Main function to export HTML table to Excel
export_xlsx <- function(html_table, 
                        file, 
                        merge_option = "both", 
                        simplify_2nd_row = FALSE,
                        colwidths = NULL,
                        verbose = FALSE,
                        rows_to_pack = NULL,
                        line_above_rows = NULL,
                        line_below_rows = NULL,
                        specify_header_rows = c(1),
                        repeat_header_rows_at = c(1)) {
  
  # Check and load required packages
  required_packages <- c("xml2", "rvest", "openxlsx")
  check_and_load_packages(required_packages)
  
  # Validate inputs
  validate_merge_option(merge_option)
  validate_file_path(file)
  
  # Read the HTML and extract the table data
  if (verbose) message("Reading HTML table...")
  html <- xml2::read_html(as.character(html_table))
  df_table <- html %>%
    rvest::html_node("table") %>%
    rvest::html_table(header = TRUE)
  
  final <- as.data.frame(df_table)
  
  # Validate colwidths option
  validate_colwidths(colwidths, ncol(final))
  
  # Simplify the second row if required
  if (simplify_2nd_row) {
    if (verbose) message("Simplifying second row...")
    if (nrow(final) > 1) {
      final[1, ] <- sapply(final[1, ], function(cell) {
        if (is.character(cell)) {
          parts <- unlist(strsplit(cell, " "))
          if (length(parts) > 1) {
            paste(parts[-length(parts)], collapse = " ")
          } else {
            cell  # If there's only one word, keep it as is
          }
        } else {
          cell  # If the cell is not a character type, keep it as is
        }
      })
    }
  }
  
  # Create a new workbook and add a worksheet
  if (verbose) message("Creating workbook...")
  wb <- createWorkbook()
  sheet_name <- "Sheet1"
  addWorksheet(wb, sheet_name)
  
  # Write the data frame to the worksheet
  writeData(wb, sheet_name, final)
  
  # Center cells in all columns besides the first
  addStyle(
    wb,
    sheet = sheet_name,
    style = createStyle(halign = "center"),
    rows = 1:nrow(final),
    cols = 2:ncol(final),
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # Convert negative row indices to positive
  convert_to_positive_rows <- function(rows, nrows) {
    if (is.null(rows)) return(NULL)
    return(ifelse(rows < 0, nrows + rows + 1, rows))
  }
  
  line_above_rows <- convert_to_positive_rows(line_above_rows, nrow(final))
  line_below_rows <- convert_to_positive_rows(line_below_rows, nrow(final))
  
  # Apply lines above specified rows
  if (!is.null(line_above_rows)) {
    for (row in line_above_rows) {
      addStyle(
        wb,
        sheet = sheet_name,
        style = createStyle(border = "Top", borderStyle = "thin"),
        rows = row + 1,  # +1 because data starts on the second row in Excel
        cols = 1:ncol(final),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }
  
  # Apply lines below specified rows
  if (!is.null(line_below_rows)) {
    for (row in line_below_rows) {
      addStyle(
        wb,
        sheet = sheet_name,
        style = createStyle(border = "Bottom", borderStyle = "thin"),
        rows = row + 1,  # +1 because data starts on the second row in Excel
        cols = 1:ncol(final),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }
  
  # Convert packing names to regex
  packing_names <- NULL
  if (!is.null(rows_to_pack)) {
    packing_names <- paste0(names(rows_to_pack), collapse = "|")
  }
  
  # Apply merging based on merge_option
  if (verbose) message("Applying cell merging...")
  if (merge_option %in% c("header", "both")) {
    merge_identical_cells(wb, sheet_name, final, 1, is_header = TRUE)
  }
  
  if (merge_option %in% c("data", "both")) {
    for (row in 2:(nrow(final) + 1)) {
      if (!is.null(packing_names) && !grepl(packing_names, final[row - 1, 1], ignore.case = TRUE)) {
        merge_identical_cells(wb, sheet_name, final, row, is_header = FALSE)
      }
    }
  }
  
  # Apply formatting to cells that contain an asterisk
  if (verbose) message("Applying formatting for asterisks...")
  for (row in 1:nrow(final)) {
    for (col in 1:ncol(final)) {
      if (grepl("\\*", final[row, col])) {
        addStyle(
          wb,
          sheet = sheet_name,
          style = createStyle(textDecoration = "bold"),
          rows = row + 1,
          cols = col,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  }
  
  # Merge cells for rows where the first column matches rows_to_pack names
  if (verbose) message("Merging cells for specific rows...")
  for (row in 1:nrow(final)) {
    if (!is.null(packing_names) && final[row, 1] %in% names(rows_to_pack)) {
      mergeCells(wb, sheet_name, cols = 1:ncol(final), rows = row + 1)
      addStyle(
        wb,
        sheet = sheet_name,
        style = createStyle(halign = "left", textDecoration = "italic"),
        rows = row + 1,
        cols = 1,
        gridExpand = TRUE,
        stack = TRUE
      )
      
      # Get the difference for indentation calculation
      if (!is.null(rows_to_pack)) {
        for (i in seq_along(rows_to_pack)) {
          title <- names(rows_to_pack)[[i]]
          values <- rows_to_pack[[i]]
          if (row >= values[1] && row <= values[2]) {
            indent_rows <- (values[2] - values[1]) + 1
            break
          }
        }
        
        if (exists("indent_rows")) {
          # Apply indentation for the rows below the packed row
          for (subsequent_row in (row + 1):(row + indent_rows)) {
            if (!is.null(packing_names) && !grepl(packing_names, final[subsequent_row, 1], ignore.case = TRUE)) {
              addStyle(
                wb,
                sheet = sheet_name,
                style = createStyle(indent = 1),
                rows = subsequent_row + 1,
                cols = 1,
                gridExpand = TRUE,
                stack = TRUE
              )
            } else {
              break
            }
          }
        }
      }
    }
  }
  
  # Set column width of first col
  if (!is.null(colwidths)) {
    if (verbose) message("Setting column widths...")
    for (i in seq_along(colwidths)) {
      setColWidths(wb, sheet = sheet_name, cols = i, widths = colwidths[i])
    }
  }
  
  # Save the workbook
  if (verbose) message("Saving workbook...")
  saveWorkbook(wb, file, overwrite = TRUE)
  if (verbose) message("Workbook saved successfully.")
}



