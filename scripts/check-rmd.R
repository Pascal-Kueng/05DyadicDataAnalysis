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

message("Rmd validation passed: purl, parse, and targeted text checks succeeded.")
