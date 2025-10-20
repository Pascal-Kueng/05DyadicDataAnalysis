#' (Step 1) Reshape Dyadic Data into Actor-Partner Format
#'
#' This is the first step in the data preparation pipeline. It takes a
#' person-level dataframe and reshapes it into the "4-field" or
#' actor-partner format, creating `_actor` and `_partner` versions of
#' specified variables. The output of this function is designed to be
#' inspected by the user before being passed to `finalize_dyadic_data()`.
#' It also attaches key variable names as attributes to the dataframe for
#' convenient use in the next step.
#'
#' @param data A dataframe in person-level format (one row per person).
#' @param person_id A string specifying the name of the unique person ID column.
#' @param dyad_id A string specifying the name of the dyad/cluster ID column.
#' @param vars_to_reshape A character vector of all variable names that need
#'  to be available in both actor and partner format for defining group roles
#'  or for the final analysis.
#' @param time_var A string specifying the name of the time variable for
#'  longitudinal data. Defaults to NULL for cross-sectional data.
#' @param exclude_incomplete A logical flag. If FALSE (default), the function
#'  will stop with an error if any dyads do not have exactly 2 members. If
#'  TRUE, it will exclude these dyads and proceed with a warning.
#'
#' @return A tibble with the reshaped data, containing new `_actor` and
#'  `_partner` columns, and with ID variables stored as attributes.
#' @export
reshape_dyadic_data <- function(data,
                                person_id,
                                dyad_id,
                                vars_to_reshape,
                                time_var = NULL,
                                exclude_incomplete = FALSE) {
  
  # --- 1. Input Validation and Dependency Checks ---
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The 'tidyverse' package is required. Please install it.", call. = FALSE)
  }
  library(tidyverse)
  
  required_cols <- c(person_id, dyad_id, vars_to_reshape)
  if (!is.null(time_var)) required_cols <- c(required_cols, time_var)
  
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    stop("The following specified columns are not in the data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  if (is.null(time_var)) {
    person_counts <- data %>% count(!!rlang::sym(person_id))
    if (any(person_counts$n > 1)) {
      stop("Multiple observations found per person, but no 'time_var' was specified. If this is longitudinal data, please provide the time variable.", call. = FALSE)
    }
  } else {
    person_time_counts <- data %>% count(!!rlang::sym(person_id), !!rlang::sym(time_var))
    if (any(person_time_counts$n > 1)) {
      stop("Multiple observations found for the same person at the same time point. Please clean the data.", call. = FALSE)
    }
  }
  
  dyad_check <- data %>%
    group_by(across(all_of(c(dyad_id, time_var)))) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n != 2)
  
  if (nrow(dyad_check) > 0) {
    incomplete_ids <- unique(dyad_check[[dyad_id]])
    if (exclude_incomplete) {
      warning("Excluding incomplete dyads (not size 2) with the following IDs: ",
              paste(incomplete_ids, collapse = ", "), call. = FALSE)
      data <- data %>% filter(!(!!rlang::sym(dyad_id) %in% incomplete_ids))
    } else {
      stop("All dyads must have exactly 2 members at all time points for reshaping. Found incomplete dyads with IDs: ",
           paste(incomplete_ids, collapse = ", "), ". Use exclude_incomplete = TRUE to remove them.", call. = FALSE)
    }
  }
  
  # --- 2. Reshape Data ---
  grouping_vars <- c(dyad_id, time_var)
  message("Reshaping variables into actor-partner format...")
  message("NOTE: 'vars_to_reshape' must include any variable whose _actor or _partner version you intend to use in your group definitions (e.g., 'gender', 'age').")
  
  reshaped_data <- as_tibble(data) %>%
    group_by(across(all_of(grouping_vars))) %>%
    mutate(across(
      all_of(vars_to_reshape),
      list(
        actor = ~ .x,
        partner = ~ if_else(row_number() == 1, last(.x), first(.x))
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    ungroup()
  
  # --- 3. Prepare Informative Summary for Printing ---
  message("\nReshaping complete. Inspect the new '_actor' and '_partner' columns below.")
  
  new_cols <- names(reshaped_data)[grepl("_actor$|_partner$", names(reshaped_data))]
  cols_to_print <- c(person_id, dyad_id, time_var, new_cols)
  other_cols_present <- any(!names(reshaped_data) %in% cols_to_print)
  sample_to_print <- tibble() # Initialize empty sample
  
  if (!is.null(time_var) && nrow(reshaped_data) > 0) {
    message("Printing a sample of the reshaped longitudinal data for the first two individuals:")
    first_two_persons <- unique(reshaped_data[[person_id]])[1:2]
    person1_sample <- reshaped_data %>% filter(!!rlang::sym(person_id) == first_two_persons[1]) %>% slice_head(n = 3)
    person2_sample <- reshaped_data %>% filter(!!rlang::sym(person_id) == first_two_persons[2]) %>% slice_head(n = 3)
    separator_row <- person1_sample[1, ] %>% mutate(across(everything(), ~ "..."))
    person1_sample_char <- person1_sample %>% mutate(across(everything(), as.character))
    person2_sample_char <- person2_sample %>% mutate(across(everything(), as.character))
    # FIX: Explicitly use dplyr::select
    sample_to_print <- bind_rows(person1_sample_char, separator_row, person2_sample_char) %>% dplyr::select(any_of(cols_to_print))
  } else if (nrow(reshaped_data) > 0) {
    message("Printing a sample of the reshaped cross-sectional data with key columns:")
    # FIX: Explicitly use dplyr::select
    sample_to_print <- head(dplyr::select(reshaped_data, any_of(cols_to_print)))
  }
  
  if (other_cols_present && nrow(sample_to_print) > 0) {
    sample_to_print <- sample_to_print %>% mutate(`...` = "...")
  }
  
  # --- 4. Print the Summary (Context-Aware) ---
  if (nrow(sample_to_print) > 0) {
    # Check if the function is being run by knitr for an R Markdown document
    if (isTRUE(getOption("knitr.in.progress"))) {
      # If yes, use knit_print() which respects document output options (like paged tables)
      print(print_df(sample_to_print))
    } else {
      # If no (e.g., running in the R console), use the standard print for interactive viewing
      print(sample_to_print, n = if (!is.null(time_var)) 7 else 6)
    }
  }
  
  # --- 5. Attach IDs and Provide Next Steps ---
  attr(reshaped_data, "person_id") <- person_id
  attr(reshaped_data, "dyad_id") <- dyad_id
  if (!is.null(time_var)) attr(reshaped_data, "time_var") <- time_var
  
  message("\n--- NEXT STEP ---")
  message("Use the reshaped data frame as input for finalize_dyadic_data().")
  message("Example: definitions <- list(
           gender_actor == gender_partner ~ 'ss_person',
           gender_actor == 1 ~ 'het_female',
           gender_actor == 2 ~ 'het_male'
         )")
  
  return(reshaped_data)
}



#' (Step 2) Assign Dyadic Roles, Create Dummies, and Center Variables
#'
#' This is the second step in the data preparation pipeline. It takes the
#' reshaped data from `reshape_dyadic_data()` and uses user-defined formulas
#' to create the final 'group_role' variable. It then automatically creates
#' dummy variables (0/1) for each level of 'group_role'. It can also
#' perform within-between person centering for longitudinal data.
#'
#' @param reshaped_data A dataframe that has already been processed by `reshape_dyadic_data()`.
#' @param group_definitions A list of two-sided formulas in `case_when()` style
#'  (e.g., `list(condition ~ "value", condition2 ~ "value2")`).
#' @param wb_center A logical flag. If TRUE, performs within-between person
#'  centering on time-varying variables. Requires longitudinal data and the 'bmlm' package.
#' @param person_id,dyad_id,time_var Optional strings for ID columns. If NULL,
#'  the function will attempt to read them from the attributes of `reshaped_data`.
#'
#' @return A tibble with the final, analysis-ready data.
#' @export
finalize_dyadic_data <- function(reshaped_data,
                                 group_definitions,
                                 wb_center = FALSE,
                                 person_id = NULL,
                                 dyad_id = NULL,
                                 time_var = NULL) {
  
  # --- 1. Input Validation and Dependency Checks ---
  if (!requireNamespace("tidyverse", quietly = TRUE)) { stop("The 'tidyverse' package is required.", call. = FALSE) }
  if (!requireNamespace("rlang", quietly = TRUE)) { stop("The 'rlang' package is required.", call. = FALSE) }
  if (wb_center && !requireNamespace("bmlm", quietly = TRUE)) { stop("The 'bmlm' package is required for wb_center=TRUE.", call. = FALSE) }
  library(tidyverse)
  
  person_id <- person_id %||% attr(reshaped_data, "person_id")
  dyad_id <- dyad_id %||% attr(reshaped_data, "dyad_id")
  time_var <- time_var %||% attr(reshaped_data, "time_var")
  
  if (is.null(person_id) || is.null(dyad_id)) {
    stop("Could not find person_id or dyad_id. Please provide them as arguments.", call. = FALSE)
  }
  if (wb_center && is.null(time_var)) {
    stop("A 'time_var' must be available for within-between centering.", call. = FALSE)
  }
  
  # --- 2. Dynamically Create 'group_role' ---
  message("Creating 'group_role' variable from definitions...")
  final_data <- reshaped_data
  final_data$group_role <- NA_character_
  
  for (i in seq_along(group_definitions)) {
    condition_formula <- group_definitions[[i]]
    condition_expr <- rlang::f_lhs(condition_formula)
    value_str <- rlang::f_rhs(condition_formula)
    # Evaluate the condition and update only rows that are currently NA
    rows_to_update <- which(rlang::eval_tidy(condition_expr, data = final_data) & is.na(final_data$group_role))
    if (length(rows_to_update) > 0) final_data$group_role[rows_to_update] <- value_str
  }
  
  if(any(is.na(final_data$group_role))) {
    warning("Some rows were not assigned to a group and have NA in 'group_role'. Check your group_definitions.", call. = FALSE)
  }
  
  # --- 3. Create Dummy Variables for group_role ---
  message("Creating dummy variables for each level of 'group_role'...")
  group_levels <- final_data %>%
    pull(group_role) %>%
    na.omit() %>%
    unique()
  
  if (length(group_levels) > 0) {
    for (level in group_levels) {
      # Sanitize level name to be a valid column name if necessary
      clean_level <- make.names(level)
      new_col_name <- paste0("is_", clean_level)
      
      # Use the original level for comparison
      final_data <- final_data %>%
        mutate(!!new_col_name := if_else(group_role == level, 1, 0))
    }
    new_cols_created <- paste0("is_", make.names(group_levels), collapse = ", ")
    message("Created dummy columns: ", new_cols_created)
  } else {
    warning("No group levels found in 'group_role'. No dummy variables were created.", call. = FALSE)
  }
  
  # --- 4. Perform Within-Between Centering (Optional) ---
  if (wb_center) {
    message("Performing within-between person centering...")
    reshaped_vars_base <- names(final_data) %>%
      str_extract(".*(?=_(actor|partner)$)") %>%
      na.omit() %>%
      unique()
    
    time_varying_vars <- reshaped_vars_base[sapply(reshaped_vars_base, function(var) {
      final_data %>%
        group_by(!!rlang::sym(person_id)) %>%
        summarise(variance = var(!!rlang::sym(var), na.rm = TRUE)) %>%
        pull(variance) %>%
        any(. > 0, na.rm = TRUE)
    })]
    
    if(length(time_varying_vars) > 0) {
      message("Centering the following time-varying variables: ", paste(time_varying_vars, collapse = ", "))
      cols_to_center_actor_partner <- names(final_data) %>%
        grep(paste0("^(", paste(time_varying_vars, collapse="|"), ")_(actor|partner)$"), ., value = TRUE)
      
      final_data <- final_data %>%
        bmlm::isolate(by = person_id, value = all_of(cols_to_center_actor_partner), which = "both")
    } else {
      message("No time-varying variables identified to center.")
    }
  }
  
  # --- 5. Prepare Informative Summary for Printing ---
  message("\nFinal data preparation complete.")
  
  id_cols <- c(person_id, dyad_id, time_var)
  dummy_cols <- names(final_data)[startsWith(names(final_data), "is_")]
  
  if (wb_center) {
    data_cols <- names(final_data)[grepl("_cw$|_cb$", names(final_data))]
  } else {
    data_cols <- names(final_data)[grepl("_actor$|_partner$", names(final_data))]
  }
  
  cols_to_print <- c(id_cols, "group_role", dummy_cols, data_cols)
  other_cols_present <- any(!names(final_data) %in% cols_to_print)
  sample_to_print <- tibble()
  
  if (!is.null(time_var) && nrow(final_data) > 0) {
    message("Printing a sample of the final longitudinal data:")
    first_two_persons <- unique(final_data[[person_id]])[1:2]
    person1_sample <- final_data %>% filter(!!rlang::sym(person_id) == first_two_persons[1]) %>% slice_head(n = 3)
    person2_sample <- final_data %>% filter(!!rlang::sym(person_id) == first_two_persons[2]) %>% slice_head(n = 3)
    separator_row <- person1_sample[1, ] %>% mutate(across(everything(), ~ "..."))
    person1_sample_char <- person1_sample %>% mutate(across(everything(), as.character))
    person2_sample_char <- person2_sample %>% mutate(across(everything(), as.character))
    sample_to_print <- bind_rows(person1_sample_char, separator_row, person2_sample_char) %>% dplyr::select(any_of(cols_to_print))
  } else if (nrow(final_data) > 0) {
    message("Printing a sample of the final cross-sectional data:")
    sample_to_print <- head(dplyr::select(final_data, any_of(cols_to_print)))
  }
  
  if (other_cols_present && nrow(sample_to_print) > 0) {
    sample_to_print <- sample_to_print %>% mutate(`...` = "...")
  }
  
  # --- 6. Print the Summary (Context-Aware) ---
  if (nrow(sample_to_print) > 0) {
    if (isTRUE(getOption("knitr.in.progress"))) {
      print(knitr::kable(sample_to_print))
    } else {
      print(sample_to_print, n = if (!is.null(time_var)) 7 else 6)
    }
  }
  
  return(final_data)
}