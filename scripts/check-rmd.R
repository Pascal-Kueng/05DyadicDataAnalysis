#!/usr/bin/env Rscript

rmd_file <- "DyadicDataAnalysis.Rmd"

if (!file.exists(rmd_file)) {
  stop("Cannot find ", sQuote(rmd_file), ". Run from the repository root.", call. = FALSE)
}

if (!requireNamespace("knitr", quietly = TRUE)) {
  stop("Package 'knitr' is required for this check.", call. = FALSE)
}

extracted_r <- tempfile(fileext = ".R")
invisible(knitr::purl(rmd_file, output = extracted_r, documentation = 0, quiet = TRUE))
invisible(parse(extracted_r))

lines <- readLines(rmd_file, warn = FALSE)

find_matches <- function(pattern, text = lines, perl = TRUE) {
  hits <- grep(pattern, text, perl = perl)
  data.frame(
    line = hits,
    text = text[hits],
    stringsAsFactors = FALSE
  )
}

validate_code_line_numbers <- function(text = lines) {
  starts <- grep("^```\\{r", text)
  ends <- grep("^```\\s*$", text)
  failures <- list()

  for (start in starts) {
    end <- ends[ends > start][1]
    if (is.na(end) || end <= start + 1) {
      next
    }

    body <- text[(start + 1):(end - 1)]
    option_index <- grep("^#\\|\\s*code-line-numbers\\s*:", body)
    if (length(option_index) == 0) {
      next
    }

    code <- body[!grepl("^#\\|", body)]
    max_line <- length(code)

    for (option in option_index) {
      value <- sub("^#\\|\\s*code-line-numbers\\s*:\\s*", "", body[option])
      value <- gsub("^[\"']|[\"']$", "", trimws(value))
      if (value %in% c("true", "false", "TRUE", "FALSE")) {
        next
      }

      ranges <- strsplit(value, "\\|", fixed = FALSE)[[1]]
      for (range in ranges) {
        range <- trimws(range)
        if (!grepl("^[0-9]+(-[0-9]+)?$", range)) {
          failures[[length(failures) + 1]] <- data.frame(
            line = start + option,
            text = paste("Invalid code-line-numbers range:", range),
            stringsAsFactors = FALSE
          )
          next
        }

        bounds <- as.integer(strsplit(range, "-", fixed = TRUE)[[1]])
        if (length(bounds) == 1) {
          bounds <- rep(bounds, 2)
        }

        if (bounds[1] > bounds[2] || bounds[1] < 1 || bounds[2] > max_line) {
          failures[[length(failures) + 1]] <- data.frame(
            line = start + option,
            text = paste0(
              "code-line-numbers range ", range,
              " is outside chunk code lines 1-", max_line
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(failures) == 0) {
    return(NULL)
  }
  do.call(rbind, failures)
}

checks <- list(
  TODO = "TODO",
  `!TODO` = "!TODO",
  model1_glmmTMB = "\\bmodel1_glmmTMB\\b",
  model_ind_apim = "\\bmodel_ind_apim\\b",
  model_ind_dim = "\\bmodel_ind_dim\\b",
  `summary_apim assignment` = "^\\s*summary_apim\\s*<-\\s*summarize_brms\\(",
  `summary_dim assignment` = "^\\s*summary_dim\\s*<-\\s*summarize_brms\\(",
  `generic formula assignment` = "^\\s*formula\\s*<-",
  `generic priors assignment` = "^\\s*priors\\s*<-",
  `generic priors_simple assignment` = "^\\s*priors_simple\\s*<-",
  `generic priors_complex assignment` = "^\\s*priors_complex\\s*<-"
)

failures <- lapply(names(checks), function(name) {
  matches <- find_matches(checks[[name]])
  if (nrow(matches) == 0) {
    return(NULL)
  }
  matches$check <- name
  matches[, c("check", "line", "text")]
})
failures <- do.call(rbind, failures)

line_number_failures <- validate_code_line_numbers()
if (!is.null(line_number_failures) && nrow(line_number_failures) > 0) {
  line_number_failures$check <- "code-line-numbers"
  line_number_failures <- line_number_failures[, c("check", "line", "text")]
  failures <- rbind(failures, line_number_failures)
}

if (!is.null(failures) && nrow(failures) > 0) {
  message("Rmd validation failed:")
  for (i in seq_len(nrow(failures))) {
    message(
      sprintf(
        "- %s at %s:%d: %s",
        failures$check[i],
        rmd_file,
        failures$line[i],
        trimws(failures$text[i])
      )
    )
  }
  quit(status = 1)
}

message("Rmd validation passed: purl, parse, code-line-numbers, and targeted text checks succeeded.")
