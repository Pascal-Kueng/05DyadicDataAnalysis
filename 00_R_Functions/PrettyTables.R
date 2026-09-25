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
# 2. print_wide_matrix
###################################
# Description:
# This function prints wide covariance and correlation matrices using the
# regular slide styling in HTML and compact, scaled LaTeX styling in PDF.




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

# Shorten technical labels so wide model tables remain readable in the PDF.
clean_pdf_label <- function(x) {
  x <- gsub("provided_support", "support", x, fixed = TRUE)
  x <- gsub("communication", "comm", x, fixed = TRUE)
  x <- gsub("diaryday", "day", x, fixed = TRUE)
  x <- gsub("_gmc", "", x, fixed = TRUE)
  x <- gsub("_cbp", " BP", x, fixed = TRUE)
  x <- gsub("_cwp", " WP", x, fixed = TRUE)
  x <- gsub("_cw", " W", x, fixed = TRUE)
  x <- gsub("_cb", " B", x, fixed = TRUE)
  x <- gsub("_", " ", x, fixed = TRUE)
  x <- gsub(":", ": ", x, fixed = TRUE)
  gsub(",", ", ", x, fixed = TRUE)
}

# Main function for format-aware kable printing
print_df <- function(df,
                     caption = NULL,
                     digits = NULL,
                     width = NULL,              # <- set NULL to avoid forcing narrow columns
                     rows_to_pack = NULL,
                     scroll_height = "auto",    # "auto" | numeric (px) | CSS length ("50vh", "20rem")
                     scroll_width  = "100%",
                     font_size = if (knitr::is_latex_output()) 7 else 20) {
  
  normalize_css_len <- function(x, default = "auto") {
    if (is.null(x)) return(default)
    if (is.numeric(x) && length(x) == 1 && !is.na(x)) return(paste0(x, "px"))
    as.character(x)
  }
  
  required_packages <- c("knitr", "kableExtra", "dplyr")
  check_and_load_packages(required_packages)
  validate_packing_input(rows_to_pack, nrow(df))

  if (knitr::is_latex_output()) {
    df <- as.data.frame(df)
    if (!identical(rownames(df), as.character(seq_len(nrow(df))))) {
      df <- tibble::rownames_to_column(df, var = " ")
    }

    names(df) <- clean_pdf_label(names(df))

    if (!is.null(digits)) {
      numeric_cols <- vapply(df, is.numeric, logical(1))
      df[numeric_cols] <- lapply(df[numeric_cols], round, digits = digits)
    }

    character_cols <- vapply(df, is.character, logical(1))
    df[character_cols] <- lapply(df[character_cols], clean_pdf_label)

    use_longtable <- nrow(df) > 28
    wide_table <- ncol(df) >= 5 ||
      any(vapply(df, function(x) {
        is.character(x) && max(nchar(x), na.rm = TRUE) > 22
      }, logical(1)))
    alignments <- ifelse(vapply(df, is.numeric, logical(1)), "r", "l")

    tbl <- knitr::kable(
      df,
      caption = caption,
      escape = FALSE,
      align = alignments,
      row.names = FALSE,
      booktabs = TRUE,
      longtable = use_longtable
    )

    if (use_longtable) {
      tbl <- tbl |>
        kableExtra::kable_styling(
          font_size = min(font_size, 7),
          latex_options = "repeat_header",
          position = "left"
        )
    } else {
      latex_options <- if (wide_table && is.null(rows_to_pack)) {
        "scale_down"
      } else {
        character()
      }
      if (length(latex_options) > 0) {
        tbl <- suppressWarnings(
          tbl |>
            kableExtra::kable_styling(
              font_size = font_size,
              latex_options = latex_options,
              position = "left"
            )
        )
      } else {
        tbl <- tbl |>
          kableExtra::kable_styling(
            font_size = font_size,
            position = "left"
          )
      }
    }

    if (use_longtable && ncol(df) >= 4) {
      tbl <- tbl |>
        kableExtra::column_spec(1, width = "13em")
    }

    return(packing(tbl, rows_to_pack))
  }
  
  if (!is.null(digits)) {
    df <- df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits)))
  }
  
  alignments <- ifelse(sapply(df, is.numeric), "r", "l")
  
  kbl <- knitr::kable(
    df,
    caption = caption,
    escape = FALSE,
    align = alignments
  ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped"),
      full_width = FALSE,
      fixed_thead = TRUE,
      font_size = font_size,
      position = "left"
    ) |>
    # prevent header wrap
    kableExtra::row_spec(0, extra_css = "white-space: nowrap;")
  
  # prevent body-wrap (+ padding); avoid forcing widths if width = NULL
  if (is.null(width)) {
    kbl <- kbl |>
      kableExtra::column_spec(1:ncol(df), extra_css = "white-space: nowrap; padding: 10px;")
  } else {
    kbl <- kbl |>
      kableExtra::column_spec(1:ncol(df), width = width, extra_css = "white-space: nowrap; padding: 10px;")
  }
  
  kbl <- kbl |>
    kableExtra::scroll_box(
      width  = normalize_css_len(scroll_width, "100%"),
      height = normalize_css_len(scroll_height, "auto")
    )
  
  kbl <- packing(kbl, rows_to_pack)
  return(kbl)
}

# Print covariance/correlation matrices compactly in PDF and normally in HTML.
print_wide_matrix <- function(x, digits = 3, font_size = 6) {
  if (!knitr::is_latex_output()) {
    return(print_df(as.data.frame(round(x, digits)), digits = digits))
  }

  df <- as.data.frame(round(x, digits))
  if (!identical(rownames(df), as.character(seq_len(nrow(df))))) {
    df <- tibble::rownames_to_column(df, var = " ")
  }

  clean_matrix_label <- function(x) {
    x <- clean_pdf_label(x)
    x <- gsub("PartnerA", "PA", x, fixed = TRUE)
    x <- gsub("PartnerB", "PB", x, fixed = TRUE)
    x <- gsub("Intercept", "Int", x, fixed = TRUE)
    x <- gsub("support", "sup", x, fixed = TRUE)
    x <- gsub("actor", "act", x, fixed = TRUE)
    gsub("partner", "prt", x, fixed = TRUE)
  }

  names(df) <- clean_matrix_label(names(df))
  df <- df |>
    dplyr::mutate(dplyr::across(where(is.character), clean_matrix_label))

  suppressWarnings(
    knitr::kable(
      df,
      format = "latex",
      escape = FALSE,
      row.names = FALSE,
      booktabs = TRUE,
      linesep = rep("", max(nrow(df) - 1, 0)),
      align = ifelse(vapply(df, is.numeric, logical(1)), "r", "l")
    ) |>
      kableExtra::kable_styling(
        font_size = font_size,
        latex_options = "scale_down",
        position = "left"
      )
  )
}
