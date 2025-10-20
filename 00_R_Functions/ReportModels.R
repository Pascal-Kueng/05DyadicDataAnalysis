# Main Functions in this file (3):

###################################
# 1. my_brm
###################################
# Description:
# This function fits a Bayesian regression model using the `brms` package, with the option to include multiple imputed datasets.
# If multiple imputation is specified, the function fits the model across all imputed datasets using parallel processing. 
# The function can save the model object to a file and provides feedback on model convergence based on Rhat values.
#
# Arguments:
# - data: The primary dataset to be used for modeling.
# - imputed_data: A list of imputed datasets (required if mi = TRUE).
# - mi: A logical value indicating whether to perform multiple imputation. Default is FALSE.
# - file: Optional. A string specifying the file path to save the model object.
# - ...: Additional arguments to be passed to the `brm` or `brm_multiple` function.
#
# Example:
# ```r
# model <- my_brm(data = my_data, imputed_data = my_imputed_data, mi = TRUE, file = "my_model")
# ```

###################################
# 1.5 evaluate_model
###################################


###################################
# 2. summarize_brms
###################################
# Description:
# This function summarizes the fixed and random effects from a Bayesian regression model fitted using the `brms` package.
# It can provide a detailed summary or a shortened version, and allows for the exponentiation of coefficients if needed.
# The function also formats the output for easy interpretation, including the addition of significance stars based on credible intervals.
#
# Arguments:
# - model: The fitted Bayesian model object from the `brms` package.
# - short_version: A logical value indicating whether to return a shortened summary. Default is FALSE.
# - exponentiate: A logical value indicating whether to exponentiate the coefficients. Default is FALSE.
# - model_rows_fixed: Optional. A vector specifying which rows of the fixed effects summary to include.
# - model_rows_random: Optional. A vector specifying which rows of the random effects summary to include.
# - model_rownames_fixed: Optional. A vector specifying the row names for the fixed effects summary.
# - model_rownames_random: Optional. A vector specifying the row names for the random effects summary.
#
# Example:
# ```r
# summary <- summarize_brms(model = my_model, short_version = TRUE, exponentiate = TRUE)
# ```

###################################
# 3. report_side_by_side
###################################
# Description:
# This function generates a side-by-side comparison of summaries from multiple Bayesian models fitted using the `brms` package.
# It supports the comparison of both fixed and random effects across models, and can exponentiate coefficients based on the model type.
#
# Arguments:
# - ...: One or more fitted Bayesian model objects from the `brms` package.
# - model_rows_fixed: Optional. A vector specifying which rows of the fixed effects summary to include across all models.
# - model_rows_random: Optional. A vector specifying which rows of the random effects summary to include across all models.
# - model_rownames_fixed: Optional. A vector specifying the row names for the fixed effects summary across all models.
# - model_rownames_random: Optional. A vector specifying the row names for the random effects summary across all models.
#
# Example:
# ```r
# comparison <- report_side_by_side(model1, model2, model_rows_fixed = c("Intercept", "Variable1"))
# ```


# Function for modelling where we can specify whether to use MI or not. 
my_brm <- function(data, imputed_data = NULL, mi = FALSE, file = NULL, ...) {
  if (mi) {
    if (is.null(imputed_data)) {
      stop("Imputed data not provided.")
    }
    if (!is.null(file)) {
      file <- paste0(file, "_imputed")
    }
    library(future)
    plan(multisession, workers = parallel::detectCores(logical = FALSE))
    
    model_object <- brm_multiple(data = imputed_data, file = file, ...)
    print("Rhats of all imputed models range:")
    print(range(model_object$rhats))
    if(max(model_object$rhats) > 1.1) {
      message("Model might not have converged")
    } else {
      message("Model likely converged")
    }
    return(model_object)
  } else {
    return(brm(data = data, file = file, ...))
  }
}

###########################################################################
####################### REPORT MODELS #####################################
###########################################################################


summarize_brms <- function(model, 
                           exponentiate = FALSE,
                           invert_zero_component_OR = TRUE,
                           side_by_side_components = TRUE,
                           multivariate_outcomes = c(),
                           stats_to_report = c('CI', 'SE', 'pd', 'ROPE', 'BF', 'Rhat', 'ESS'),
                           include_correlations = TRUE,
                           rope_range = NULL,
                           hu_rope_range = NULL,
                           pd_significance = TRUE, # otherwise CI is used
                           alpha = c(0.05, 0.01, 0.001),
                           one_tailed = FALSE, # only an option if pd_significance is TRUE
                           
                           model_rows_fixed = NULL,
                           model_rows_random = NULL,
                           model_rownames_fixed = NULL,
                           model_rownames_random = NULL
) {
  
  format_number <- function(x, digits = 2) format(round(x, digits), nsmall = digits)
  
  # determine type of model
  is_multivariate <- any(class(model$family) == 'list')
  is_hurdle_model <- any(grepl("^hurdle_", model$family$family))
  is_zi_model <- any(grepl("^zero_inflated_", model$family$family))
  is_cumulative_model <- any(model$family$family == 'cumulative')
  
  if (is_hurdle_model) {
    prefix_to_grep <- 'hu_'
    suffix_to_add <- '_hu'
  } else if (is_zi_model) {
    prefix_to_grep <- 'zi_'
    suffix_to_add <- '_zi'
  }
  
  if ('ROPE' %in% stats_to_report & is_multivariate) {
    warning("ROPE is not yet supported for multivariate models and was excluded")
    stats_to_report <- stats_to_report[stats_to_report != 'ROPE']
  }
  
  # Extract summaries
  summ_og <- summary(model, robust = TRUE)
  fixed_effects <- summ_og$fixed
  
  # Identify and separate fixed effects that belong to the sigma model.
  # brms names these parameters with 'sigma_' for factors or 'b_sigma_' for predictors.
  sigma_fe_indices <- grep("sigma_", rownames(fixed_effects))
  
  sigma_pars <- NULL
  if (length(sigma_fe_indices) > 0) {
    # Extract these sigma parameters into a temporary data frame
    sigma_pars <- fixed_effects[sigma_fe_indices, , drop = FALSE]
    
    # Remove them from the main fixed effects data frame
    fixed_effects <- fixed_effects[-sigma_fe_indices, , drop = FALSE]
  }
  
  # 3. Process random-effects parameters
  random_list <- summ_og$random
  random_group_pars <- NULL # Initialize as NULL
  
  # Check if there are any random effects in the model
  if (length(random_list) > 0) {
    
    # Logic for models with multiple grouping factors (e.g., 3-level)
    if (length(random_list) > 1) {
      processed_random_list <- lapply(names(random_list), function(group_name) {
        re_df <- random_list[[group_name]]
        # Prefix rownames with the grouping factor name for clarity
        rownames(re_df) <- paste(group_name, rownames(re_df), sep = ": ")
        return(re_df)
      })
      random_group_pars <- do.call(rbind, processed_random_list)
      
    } else {
      # Logic for models with a single grouping factor (2-level)
      random_group_pars <- random_list[[1]]
    }
    
    # Conditionally remove correlation parameters from the random effects part
    if (!include_correlations && !is.null(random_group_pars)) {
      # Keep rows that do NOT start with 'cor('
      keep_rows <- !grepl("^cor\\(", rownames(random_group_pars))
      random_group_pars <- random_group_pars[keep_rows, , drop = FALSE]
    }
  }
  
  # 4. Combine all non-fixed-effect parameters
  if (include_correlations) {
    # Include all parameter types: random group pars, fixed cors, special pars, residual cors
    random_effects <- rbind(random_group_pars, summ_og$cor_pars, summ_og$spec_pars, summ_og$rescor_pars, sigma_pars)
    
  } else {
    # Exclude correlation-related parameter types
    random_effects <- rbind(random_group_pars, summ_og$spec_pars)
  }
  
  ###########################################################################
  ### NEW CODE: CAPTURE PARAMETER NAMES FOR LATER INDEXING ##################
  ###########################################################################
  captured_fe_names <- rownames(fixed_effects)
  captured_rgp_names <- rownames(random_group_pars)
  
  # Capture names of other parameters that were combined into `random_effects`
  captured_other_names <- character(0)
  if (include_correlations) {
    if (!is.null(summ_og$cor_pars)) captured_other_names <- c(captured_other_names, rownames(summ_og$cor_pars))
    if (!is.null(summ_og$rescor_pars)) captured_other_names <- c(captured_other_names, rownames(summ_og$rescor_pars))
  }
  if (!is.null(summ_og$spec_pars)) captured_other_names <- c(captured_other_names, rownames(summ_og$spec_pars))
  if (!is.null(sigma_pars)) captured_other_names <- c(captured_other_names, rownames(sigma_pars))
  
  ###########################################################################
  
  # Add p_direction to fixed effects
  if ('pd' %in% stats_to_report | pd_significance) {
    # 1. Get pd for ALL fixed effects (regular ones + sigma ones)
    p_dir_full <- as.data.frame(bayestestR::p_direction(
      model,
      effects = 'fixed',
      component = 'all'
    ))
    
    # 2. Create a lookup map for easy matching (Parameter -> pd)
    pd_map <- setNames(format_number(p_dir_full$pd, 3), p_dir_full$Parameter)
    
    # 3. Initialize the 'pd' column in both data frames
    fixed_effects$pd <- NA_character_
    random_effects$pd <- NA_character_
    
    # 4. Find which parameters in our data frames have a pd value
    fe_params_with_pd <- intersect(rownames(fixed_effects), names(pd_map))
    re_params_with_pd <- intersect(rownames(random_effects), names(pd_map))
    
    # 5. Assign the pd values by name
    fixed_effects[fe_params_with_pd, "pd"] <- pd_map[fe_params_with_pd]
    random_effects[re_params_with_pd, "pd"] <- pd_map[re_params_with_pd]
  }
  
  # Add ROPE
  if ('ROPE' %in% stats_to_report) {
    compute_rope <- function(range) {
      
      vifs <- performance::check_collinearity(model)
      if (max(vifs$VIF) > 10) {
        warning('Collinearity detected. Some VIFs are > 10. This may invalidate ROPE inferences!')
        print(vifs)
      }
      
      # Compute ROPE
      rope_df <- as.data.frame(
        suppressWarnings(
          bayestestR::rope(
            model,
            effects = 'fixed',
            component = 'all',
            range = range,
            ci = 1,
            verbose = FALSE
          )
        )
      )
      
      # Format ROPE
      if (exponentiate) {
        rope_df$ROPE_low <-exp(rope_df$ROPE_low)
        rope_df$ROPE_high <- exp(rope_df$ROPE_high)
      } 
      
      fixed_effects$ROPE <- paste0(
        '[',
        format_number(rope_df$ROPE_low), 
        ', ',
        format_number( rope_df$ROPE_high),
        ']'
      )
      fixed_effects$`inside ROPE` <- format_number(rope_df$ROPE_Percentage,3)
      return(fixed_effects)
    }
    
    fixed_effects <- compute_rope(range = rope_range)
    
    random_effects$ROPE <- NA
    random_effects$`inside ROPE` <- NA
    
    if (!is.null(hu_rope_range) & any(grepl('hu_', rownames(fixed_effects)))) {
      fixed_effects[grepl('hu_', rownames(fixed_effects)),] <- compute_rope(range = hu_rope_range)[grepl('hu_', rownames(fixed_effects)),]
    }
  }
  
  # Calculate Bayes Factor for fixed effects
  if ('BF' %in% stats_to_report) {
    bayesfac <- bayestestR::bayesfactor(
      model,
      effects = "fixed",
      component = 'all'
    )
    fixed_effects$BF <- ifelse(
      exp(bayesfac$log_BF) > 100, 
      '>100', 
      sprintf("%.3f", exp(bayesfac$log_BF))
    )
    
    # Add evidence interpretation using case_when for clarity
    fixed_effects <- fixed_effects %>%
      mutate(
        BF_Evidence = case_when(
          exp(bayesfac$log_BF) > 100          ~ "Overwhelming Evidence",
          exp(bayesfac$log_BF) > 30           ~ "Very Strong Evidence",
          exp(bayesfac$log_BF) > 10           ~ "Strong Evidence",
          exp(bayesfac$log_BF) > 3            ~ "Moderate Evidence",
          exp(bayesfac$log_BF) > 1            ~ "Weak Evidence",
          exp(bayesfac$log_BF) > 0.3          ~ "Weak Evidence for Null",
          exp(bayesfac$log_BF) > 0.1          ~ "Moderate Evidence for Null",
          exp(bayesfac$log_BF) > 0.03         ~ "Strong Evidence for Null",
          TRUE                                ~ "Very Strong Evidence for Null"
        )
      )
    
    random_effects$BF <- NA
    random_effects$BF_Evidence <- NA
  } 
  
  # compute significance stars
  ## based on pd if TRUE
  if (pd_significance) {
    if (!one_tailed) {
      alpha <- alpha / 2
    }
    # Get the numeric pd values for the rows currently in the fixed_effects data frame
    # by matching their names in the p_dir_full object we created earlier.
    pd_values_for_fe <- p_dir_full[rownames(fixed_effects), "pd"]
    
    # Ensure values are numeric before rounding and comparing
    significance_fixed <- dplyr::case_when(
      round(as.numeric(pd_values_for_fe), 3) >= 1 - alpha[3] ~ '***',
      round(as.numeric(pd_values_for_fe), 3) >= 1 - alpha[2] ~ '**',
      round(as.numeric(pd_values_for_fe), 3) >= 1 - alpha[1] ~ '*',
      TRUE ~ ''
    )
  } else if (!pd_significance) {
    # compute significance stars based on CI
    is_significant <- function(low, high) {
      (low > 0 & high > 0) | (low < 0 & high < 0)
    }
    significance_fixed <- ifelse(
      is_significant(fixed_effects$`l-95% CI`, fixed_effects$`u-95% CI`),
      '*', 
      ''
    )
  }
  significance_random <- NA
  
  # Rename SE
  names(fixed_effects)[2] <- "SE" 
  names(random_effects)[2] <- "SE" 
  
  if ((is_zi_model | is_hurdle_model) & invert_zero_component_OR) {
    # Create an index for the rows to invert
    invert_index <- grepl(prefix_to_grep, rownames(fixed_effects)) | grepl(prefix_to_grep, rownames(fixed_effects))
    
    # Invert the log-odds for those rows
    fixed_effects$Estimate[invert_index] <- -fixed_effects$Estimate[invert_index]
    
    # Invert and swap the bounds
    temp_upper <- -fixed_effects$`l-95% CI`[invert_index]
    temp_lower <- -fixed_effects$`u-95% CI`[invert_index]
    
    fixed_effects$`u-95% CI`[invert_index] <- temp_upper
    fixed_effects$`l-95% CI`[invert_index] <- temp_lower
  }
  
  
  # Handle exponentiation for fixed effects
  if (exponentiate) {
    fixed_effects$Estimate <- exp(fixed_effects$Estimate)
    fixed_effects$`u-95% CI` <- exp(fixed_effects$`u-95% CI`)
    fixed_effects$`l-95% CI` <- exp(fixed_effects$`l-95% CI`)
    
    # Compute SE using the delta method
    fixed_effects$SE <- fixed_effects$Estimate * fixed_effects$SE
  }
  
  # Format estimates with significance
  fixed_effects$Estimate <- ifelse(
    is.na(fixed_effects$Estimate),
    NA,
    paste0(format_number(fixed_effects$Estimate), significance_fixed)
  )
  random_effects$Estimate <- format_number(random_effects$Estimate)
  
  # Format CIs
  fixed_effects$`95% CI` <- paste0(
    '[',
    format_number(fixed_effects$`l-95% CI`), 
    ', ',
    format_number(fixed_effects$`u-95% CI`),
    ']'
  )
  random_effects$`95% CI` <- paste0(
    '[',
    format_number(random_effects$`l-95% CI`), 
    ', ',
    format_number(random_effects$`u-95% CI`),
    ']'
  )
  
  # Combine fixed and random effects
  full_results <- rbind(fixed_effects, random_effects)
  
  # Round numbers
  full_results$SE <- format_number(full_results$SE,2)
  full_results$Rhat <- format_number(full_results$Rhat,3)
  full_results$Bulk_ESS <- format_number(full_results$Bulk_ESS,0)
  full_results$Tail_ESS <- format_number(full_results$Tail_ESS,0)
  
  # Select columns
  report_names_vec <- c('Estimate')
  if ('SE' %in% stats_to_report) {
    report_names_vec <- c(report_names_vec, 'SE')
  }
  if ('CI' %in% stats_to_report) {
    report_names_vec <- c(report_names_vec, '95% CI')
  }
  if ('pd' %in% stats_to_report) {
    report_names_vec <- c(report_names_vec, 'pd')
  } 
  if ('ROPE' %in% stats_to_report) {
    report_names_vec <- c(report_names_vec, c('ROPE', 'inside ROPE'))
  }
  if ('BF' %in% stats_to_report) {
    report_names_vec <- c(report_names_vec, c('BF', 'BF_Evidence'))
  }
  if ('Rhat' %in% stats_to_report) {
    report_names_vec <- c(report_names_vec, 'Rhat')
  }
  if ('ESS' %in% stats_to_report) {
    report_names_vec <- c(report_names_vec, 'Bulk_ESS', 'Tail_ESS')
  }
  full_results_subset <- full_results[, report_names_vec]
  
  
  # Determine correct column name for Estimate.
  correct_name <- if (exponentiate) {
    if (model$family[[1]] %in% c('bernoulli', 'cumulative')) {
      'OR'
    } else if (model$family[[1]] == 'negbinomial') {
      'IRR'
    } else {
      warning("Coefficients were exponentiated. Double check if this was intended.")
      'exp(Est.)'
    }
  } else {
    'Est.'
  }
  
  names(full_results_subset)[1] <- correct_name
  
  
  # If hurdle model or zi model, put components next to each other if requested
  if ((is_hurdle_model | is_zi_model) & side_by_side_components) {
    hurdle_rows <- grepl(prefix_to_grep, rownames(full_results_subset))
    
    # Split the results into hurdle and nonzero components
    hurdle_report <- full_results_subset[hurdle_rows, ]
    nonzero_report <- full_results_subset[!hurdle_rows, ]
    
    # Remove 'hu_' from the row names of hurdle_report
    rownames(hurdle_report) <- gsub(prefix_to_grep, '', rownames(hurdle_report))
    
    # Merge the two dataframes by row names, with custom suffixes
    full_results_subset <- merge(
      hurdle_report, 
      nonzero_report, 
      by = "row.names", 
      all = TRUE, 
      suffixes = c(suffix_to_add, "_nonzero")
    )
    
    # Restore the row names from the merged dataframe and drop the temporary row name column
    rownames(full_results_subset) <- full_results_subset$Row.names
    full_results_subset$Row.names <- NULL
  }
  
  
  # Check if it's a hurdle or multivariate model
  if (!is.null(multivariate_outcomes) & is_multivariate) {
    to_grepl <- paste0(multivariate_outcomes, '_|_', multivariate_outcomes)  # Use provided list for multivariate models
    rename_str <- paste0("_", multivariate_outcomes)
    
    # If side-by-side components requested
    rescor_df <- NULL
    if (side_by_side_components & any(grepl(paste(to_grepl, collapse = "|"), rownames(fixed_effects)))) {
      
      # Create an empty dataframe for iterative merging
      formatted_results <- NULL
      rescor_df <- full_results_subset[grepl('rescor', rownames(full_results_subset)),]
      
      for (i in seq_along(to_grepl)) {
        pattern <- to_grepl[[i]]
        rename_str_i <- rename_str[[i]]
        if (!any(grepl(pattern, rownames(fixed_effects)))) {
          stop(paste(pattern, "not found in rows!"))
        }
        # Identify rows matching the current pattern
        component_rows <- grepl(pattern, rownames(full_results_subset))
        
        # Extract the component and clean row names
        component_report <- full_results_subset[component_rows, ]
        rownames(component_report) <- gsub(pattern, '', rownames(component_report))
        
        # Add back the rescor
        component_report <- rbind(component_report, rescor_df)
        
        # Add the suffix to all columns of the current component
        colnames(component_report) <- paste0(colnames(component_report), rename_str_i)
        
        # If formatted_results is NULL (first iteration), initialize it
        if (is.null(formatted_results)) {
          formatted_results <- component_report
        } else {
          # Merge with the existing formatted_results
          formatted_results <- merge(
            formatted_results,
            component_report,
            by = "row.names",
            all = TRUE
          )
          
          # Restore row names and drop temporary column
          rownames(formatted_results) <- formatted_results$Row.names
          formatted_results$Row.names <- NULL
        }
      }
      
      # Replace the full_results_subset with the formatted output
      full_results_subset <- formatted_results
    }
  }
  
  
  # Select rows and handle missing ones
  desired_rows <- c(model_rows_fixed, model_rows_random)
  if (is.null(desired_rows)) {
    desired_rows <- rownames(full_results_subset)
  }
  
  # Create an empty data frame with desired rows
  empty_df <- data.frame(
    matrix(NA, nrow = length(desired_rows), ncol = ncol(full_results_subset)),
    stringsAsFactors = FALSE
  )
  colnames(empty_df) <- colnames(full_results_subset)
  rownames(empty_df) <- desired_rows
  
  
  
  # Warn if some variables from the model are omitted
  if (!all(rownames(full_results_subset) %in% desired_rows)) {
    which_missing <- !rownames(full_results_subset) %in% desired_rows
    names_missing <- rownames(full_results_subset)[which_missing]
    warning(
      sprintf(
        "Some rows from the model were omitted due to your provided model_rows_fixed or model_rows_random vectors. Missing rows: %s", 
        paste(names_missing, collapse = ", ")
      )
    )
  }
  
  # Fill in the data where available
  available_rows <- intersect(desired_rows, rownames(full_results_subset))
  empty_df[available_rows, ] <- full_results_subset[available_rows, ]
  
  full_results_subset <- empty_df
  
  # Handle row names
  if (!is.null(model_rownames_fixed) || !is.null(model_rownames_random)) {
    model_rownames <- c(model_rownames_fixed, model_rownames_random)
    if (length(model_rownames) != nrow(full_results_subset)) {
      warning("Length of model_rownames does not match number of rows")
    } else {
      rownames(full_results_subset) <- model_rownames
    }
  }
  
  # Get the internal row names before they are potentially renamed by the user
  final_internal_names <- rownames(empty_df)
  
  row_indices <- list()
  
  # 1. Fixed Effects
  fe_indices <- which(final_internal_names %in% captured_fe_names)
  if (length(fe_indices) > 0) {
    row_indices[['Fixed Effects']] <- fe_indices
  }
  
  # 2. Random Effects (per level) - Corrected Logic
  re_groups <- names(random_list)
  if (length(re_groups) > 0) {
    # Keep track of indices that have been assigned to a group to prevent overlap
    assigned_re_indices <- c()
    
    # Process from most specific to least specific group by sorting by name length
    re_groups_sorted <- re_groups[order(nchar(re_groups), decreasing = TRUE)]
    
    for (group in re_groups_sorted) {
      # For multi-level models, names are prefixed "group: name".
      # For single-level, there's no prefix.
      if (length(re_groups) > 1) {
        group_prefix <- paste0("^", group, ":")
        re_in_group <- captured_rgp_names[grep(group_prefix, captured_rgp_names)]
      } else {
        re_in_group <- captured_rgp_names # Single level case
      }
      
      current_indices <- which(final_internal_names %in% re_in_group)
      
      # Assign only indices that have not already been claimed by a more specific group
      indices_for_this_group <- setdiff(current_indices, assigned_re_indices)
      
      if (length(indices_for_this_group) > 0) {
        level_name <- paste('Random Effects (', group, ')', sep = '')
        row_indices[[level_name]] <- sort(indices_for_this_group) # Keep indices sorted
        
        # Add these indices to the assigned pool for the next iteration
        assigned_re_indices <- c(assigned_re_indices, indices_for_this_group)
      }
    }
  }
  
  # 3. Additional Parameters
  other_indices <- which(final_internal_names %in% captured_other_names)
  if (length(other_indices) > 0) {
    row_indices[['Additional Parameters']] <- other_indices
  }
  
  # 4. Reorder list elements to match original model structure and remove empty ones
  original_order <- c(
    'Fixed Effects',
    if(exists("re_groups")) paste('Random Effects (', re_groups, ')', sep = ''),
    'Additional Parameters'
  )
  row_indices <- row_indices[intersect(original_order, names(row_indices))]
  
  # Attach the list as an attribute to the final data frame
  attr(full_results_subset, "row_indices") <- row_indices
  
  ###########################################################################
  
  return(full_results_subset)
}






# Function to report all models side by side with column subsetting.
report_side_by_side <- function(
    ..., 
    stats_to_report = c('CI', 'pd'),
    model_rows_random = NULL, 
    model_rows_fixed = NULL,
    model_rownames_fixed = NULL, 
    model_rownames_random = NULL
    ) {
  
  models <- list(...)
  model_names <- sapply(substitute(list(...))[-1], deparse)
  names(models) <- model_names
  
  side_by_side <- NULL
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model_names[i]
    print(model_name)
    if (model$family$link %in% c(
      'log', 'logit'
    ) | grepl('lognormal', model$family$family) | grepl('log', model_name)) {
      exponentiate <- TRUE
    } else {
      exponentiate <- FALSE
    }
    model_summary <- summarize_brms(
      model, 
      exponentiate = exponentiate, 
      stats_to_report = stats_to_report,
      model_rows_random = model_rows_random,
      model_rows_fixed = model_rows_fixed,
      model_rownames_fixed = model_rownames_fixed,
      model_rownames_random = model_rownames_random
    )
    
    colnames(model_summary) <- paste(colnames(model_summary), model_name)
    
    
    if (is.null(side_by_side)) {
      side_by_side <- model_summary
    } else {
      side_by_side <- cbind(side_by_side, model_summary)
    }
  }
  
  return(side_by_side)
}




############################################################################
############################ PLOT EFFECTS ##################################
############################################################################


# Main function
conditional_spaghetti <- function(
    model,                    # a brmsfit object
    effects,                  # character vector of variable names
    group_var = NULL,         # character of variable name
    n_groups = NULL,          # if NULL all slopes are plotted, else n random samples are plotted
    seed = 45,                # seed for random samples
    robust = TRUE,            # TRUE takes the median, FALSE the mean from the samples of the posterior
    plot_full_range = FALSE,  # Plot over full predictor range if TRUE instead of range with data
    x_limits = NULL,          # vector with lower and upper bound of x-axis (optional)
    y_limits = NULL,          # vector with lower and upper bound of y-axis (optional)
    x_label = NULL,           # character
    y_label = NULL,           # character
    y_labels = NULL, 
    transform_fn = NULL,       # function to transform values after inverse link
    filter_quantiles = NULL,
    font_family = 'Segoe UI',
    p_title = NULL
) {
  if (!inherits(model, 'brmsfit')) {
    stop("Only brmsfit objects supported")
  }

  # Check for required packages
  required_packages <- c("dplyr", "tidyr", "ggplot2", "posterior")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "is required but not installed."))
    }
  }
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Determine if the model is a hurdle, zero-inflated, or cumulative model
  is_hurdle_model <- grepl("^hurdle_", model$family$family)
  is_zi_model <- grepl("^zero_inflated_", model$family$family)
  is_cumulative_model <- model$family$family == 'cumulative'
  
  # Call appropriate plotting function based on model type
  if (is_cumulative_model) {
    plots <- plot_cumulative_model(
      model = model,
      effects = effects,
      group_var = group_var,
      n_groups = n_groups,
      seed = seed,
      robust = robust,
      plot_full_range = plot_full_range,
      x_limits = x_limits,
      y_limits = y_limits,
      x_label = x_label,
      y_label = y_label,
      transform_fn = transform_fn
    )
  } else if (is_hurdle_model || is_zi_model) {
    plots <- plot_hurdle_model(
      model = model,
      effects = effects,
      group_var = group_var,
      n_groups = n_groups,
      seed = seed,
      robust = robust,
      plot_full_range = plot_full_range,
      x_limits = x_limits,
      y_limits = y_limits,
      x_label = x_label,
      y_label = y_label,
      y_labels = y_labels,
      transform_fn = transform_fn,
      filter_quantiles = filter_quantiles,
      font_family = font_family,
      p_title = p_title
    )
  } else {
    plots <- plot_general_model(
      model = model,
      effects = effects,
      group_var = group_var,
      n_groups = n_groups,
      seed = seed,
      robust = robust,
      plot_full_range = plot_full_range,
      x_limits = x_limits,
      y_limits = y_limits,
      x_label = x_label,
      y_label = y_label,
      transform_fn = transform_fn
    )
  }
  
  # Return the plots
  return(plots)
}





# Function for general models
plot_general_model <- function(
    model,
    effects,
    group_var,
    n_groups,
    seed,
    robust,
    plot_full_range,
    x_limits,
    y_limits,
    x_label,
    y_label,
    transform_fn
) {
  plots_list <- list()
  
  # Extract posterior samples for fixed effects
  posterior_samples <- posterior::as_draws_df(model) %>% as.data.frame()
  
  # Extract sigma samples if necessary
  if (model$family$family == 'lognormal') {
    # Sigma parameter in lognormal models
    sigma_samples <- posterior_samples$sigma
  }
  
  for (i in seq_along(effects)) {
    e <- effects[[i]]
    
    # Define a range of values for the predictor across all data
    predictor_range <- seq(min(model$data[[e]], na.rm = TRUE), max(model$data[[e]], na.rm = TRUE), length.out = 100)
    
    # Set up labels
    x_lab <- ifelse(is.null(x_label), e, x_label[[i]])
    y_lab <- ifelse(is.null(y_label), 'Response', y_label)
    
    # Define inverse link function
    inverse_link_functions <- list(
      logit = plogis,
      probit = pnorm,
      cloglog = function(x) 1 - exp(-exp(x)),
      identity = function(x) x,
      log = exp,
      inverse = function(x) 1 / x
    )
    reverse_link <- inverse_link_functions[[model$family$link]]
    if (is.null(reverse_link)) {
      stop("Link function not recognized or not supported.")
    }
    
    # Extract fixed intercept and slope samples
    fixed_intercept_samples <- posterior_samples$b_Intercept
    fixed_slope_samples <- posterior_samples[[paste0("b_", e)]]
    
    # Handle cases where the effect is not in the fixed effects
    if (is.null(fixed_slope_samples)) {
      stop(paste("Effect", e, "not found in fixed effects.")
      )
    }
    
    # Choose mean or median based on the 'robust' argument
    if (robust) {
      # Use median for robust summaries
      fixed_intercept_central <- median(fixed_intercept_samples)
      fixed_slope_central <- median(fixed_slope_samples)
      if (model$family$family == 'lognormal') {
        sigma_central <- median(sigma_samples)
      }
    } else {
      # Use mean for traditional summaries
      fixed_intercept_central <- mean(fixed_intercept_samples)
      fixed_slope_central <- mean(fixed_slope_samples)
      if (model$family$family == 'lognormal') {
        sigma_central <- mean(sigma_samples)
      }
    }
    
    # Generate fixed effect predictions across the predictor range
    linear_predictor <- fixed_intercept_central + fixed_slope_central * predictor_range
    
    # Apply inverse link and custom transformation if provided
    if (model$family$family == 'lognormal') {
      # Expected value for lognormal: exp(mu + 0.5 * sigma^2)
      response <- exp(linear_predictor + 0.5 * sigma_central^2)
    } else {
      response <- reverse_link(linear_predictor)
    }
    if (!is.null(transform_fn)) {
      response <- transform_fn(response)
    }
    fixed_predictions <- data.frame(
      x_value = predictor_range,
      response = response
    )
    
    # Calculate credible intervals
    ci_bounds_fixed <- sapply(predictor_range, function(x_val) {
      lp <- fixed_intercept_samples + fixed_slope_samples * x_val
      if (grepl("lognormal", model$family$family)) {
        # For each sample, compute expected value
        response_samples <- exp(lp + 0.5 * sigma_samples^2)
      } else {
        response_samples <- reverse_link(lp)
      }
      if (!is.null(transform_fn)) {
        response_samples <- transform_fn(response_samples)
      }
      response_samples
    })
    fixed_predictions$lower <- apply(ci_bounds_fixed, 2, quantile, probs = 0.025)
    fixed_predictions$upper <- apply(ci_bounds_fixed, 2, quantile, probs = 0.975)
    
    # Initialize ggplot
    p <- ggplot() +
      # Add CI ribbon for the fixed effect
      geom_ribbon(
        data = fixed_predictions,
        aes(x = x_value, ymin = lower, ymax = upper),
        fill = "#1f78b4", alpha = 0.24
      ) +
      # Add fixed effect line
      geom_line(
        data = fixed_predictions,
        aes(x = x_value, y = response),
        color = "#1f78b4",
        linewidth =1.8
      ) +
      labs(
        title = paste("Fixed Effects:", x_lab),
        x = x_lab,
        y = y_lab
      ) +
      theme_bw(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    # If group_var is provided, add random effects
    if (!is.null(group_var)) {
      # Handle random effects as before
      random_effects_prefix <- paste0("r_", group_var)
      random_effects_pattern <- paste0("r_", group_var, "\\[(.*),(.+)]")
      random_effects <- posterior_samples %>%
        select(starts_with(random_effects_prefix)) %>%
        pivot_longer(
          cols = everything(),
          names_to = c("group_level", ".value"),
          names_pattern = random_effects_pattern
        ) %>%
        mutate(group_level = as.character(group_level)) %>%
        select(group_level, Intercept, starts_with(e))
      
      # Handle cases where random effects for 'e' are not present
      random_slope_name <- e
      if (!random_slope_name %in% names(random_effects)) {
        # If random slope for 'e' is not present, set it to zero
        random_effects[[random_slope_name]] <- 0
      }
      
      # Determine which group levels to include
      unique_groups <- unique(random_effects$group_level)
      if (!is.null(n_groups)) {
        if (n_groups < length(unique_groups)) {
          if (!is.null(seed)) {
            set.seed(seed)  # Set seed for reproducibility
          }
          selected_groups <- sample(unique_groups, n_groups)
        } else {
          selected_groups <- unique_groups
        }
      } else {
        selected_groups <- unique_groups
      }
      
      # Filter random effects to include only selected group levels
      random_effects <- random_effects %>% filter(group_level %in% selected_groups)
      
      # Generate individual-level predictions
      individual_predictions <- do.call(rbind, lapply(selected_groups, function(j) {
        # Filter for the posterior samples of this specific group
        individual_samples <- random_effects %>% filter(group_level == j)
        
        # Compute the combined intercept and slope for this group
        if (robust) {
          # Use median for robust summaries
          individual_intercept <- fixed_intercept_central + median(individual_samples$Intercept, na.rm = TRUE)
          individual_slope <- fixed_slope_central + median(individual_samples[[random_slope_name]], na.rm = TRUE)
        } else {
          # Use mean for traditional summaries
          individual_intercept <- fixed_intercept_central + mean(individual_samples$Intercept, na.rm = TRUE)
          individual_slope <- fixed_slope_central + mean(individual_samples[[random_slope_name]], na.rm = TRUE)
        }
        
        # Determine predictor range for this group
        if (plot_full_range) {
          # Use the full predictor range
          predictor_range_group <- predictor_range
        } else {
          # Get the data for this group
          group_data <- model$data[model$data[[group_var]] == j, ]
          
          # Check if there is data for this group
          if (nrow(group_data) == 0) {
            return(NULL)
          }
          
          # Get the range of predictor 'e' for this group
          min_e <- min(group_data[[e]], na.rm = TRUE)
          max_e <- max(group_data[[e]], na.rm = TRUE)
          
          # Handle case where min and max are the same
          if (min_e == max_e) {
            predictor_range_group <- min_e
          } else {
            predictor_range_group <- seq(min_e, max_e, length.out = 100)
          }
        }
        
        # Generate individual-level predictions
        lp <- individual_intercept + individual_slope * predictor_range_group
        if (grepl("lognormal", model$family$family)) {
          # Expected value for lognormal
          response <- exp(lp + 0.5 * sigma_central^2)
        } else {
          response <- reverse_link(lp)
        }
        if (!is.null(transform_fn)) {
          response <- transform_fn(response)
        }
        data.frame(
          group_level = j,
          x_value = predictor_range_group,
          response = response
        )
      }))
      
      # Remove any NULL elements (in case some groups had no data)
      individual_predictions <- individual_predictions %>% filter(!is.na(response))
      
      # Add individual lines to the plot
      p <- p +
        geom_line(
          data = individual_predictions,
          aes(x = x_value, y = response, group = group_level),
          color = "#1f78b4",
          linewidth = 0.4,
          alpha = 0.4
        ) +
        labs(
          title = paste("Conditional Fixed and Random Effects:", x_lab)
        )
    }
    
    # Apply x and y limits if specified
    if (!is.null(x_limits)) {
      p <- p + xlim(x_limits)
    }
    if (!is.null(y_limits)) {
      p <- p + ylim(y_limits)
    }
    
    # Store the plot
    plots_list[[e]] <- p
  }
  
  return(plots_list)
}




# Function for hurdle and zero-inflated models with adjustable layout
plot_hurdle_model <- function(
    model,
    effects,
    group_var = NULL,
    n_groups = NULL,
    seed = NULL,
    robust = FALSE,
    plot_full_range = TRUE,
    x_limits = NULL,
    y_limits = NULL,
    x_label = NULL,
    y_label = NULL,
    y_labels = NULL,
    transform_fn = NULL,
    use_pr_notation = FALSE,  # Option to switch between 'P' and 'Pr'
    filter_quantiles = NULL,
    font_family = 'Segoe UI',
    p_title = NULL
) {
  # Load required packages
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(posterior)
  library(patchwork)
  library(grid)  
  library(ggridges)
  library(extrafont)
  font_import(pattern = "Candara", prompt = FALSE)
  suppressMessages(suppressWarnings(extrafont::loadfonts()))
  
  plots_list <- list()
  
  # Extract posterior samples for fixed effects
  posterior_samples <- posterior::as_draws_df(model) %>% as.data.frame()
  
  # Extract sigma samples if necessary
  if (grepl("lognormal", model$family$family)) {
    sigma_samples <- posterior_samples$`sigma`
  }
  
  for (i in seq_along(effects)) {
    e <- effects[[i]]
    
    # Define a range of values for the predictor across all data
    predictor_range <- seq(
      min(model$data[[e]], na.rm = TRUE),
      max(model$data[[e]], na.rm = TRUE),
      length.out = 100
    )
    
    
    # Identify the hurdle or zero-inflation component
    if (grepl("^hurdle_", model$family$family)) {
      hurdle_component <- "hu"
      model_type <- "hurdle"
    } else {
      hurdle_component <- "zi"
      model_type <- "zero_inflated"
    }
    
    # Get link functions for both components
    count_link <- model$family$link
    if (!is.null(model$formula$auxiliary[[hurdle_component]]$link)) {
      hurdle_link <- model$formula$auxiliary[[hurdle_component]]$link
    } else {
      # Default link for hurdle/zi component is 'logit' in brms
      hurdle_link <- "logit"
    }
    
    # Define inverse link functions
    inverse_link_functions <- list(
      logit = plogis,
      probit = pnorm,
      cloglog = function(x) 1 - exp(-exp(x)),
      identity = function(x) x,
      log = exp,
      inverse = function(x) 1 / x
    )
    reverse_link_count <- inverse_link_functions[[count_link]]
    reverse_link_hurdle <- inverse_link_functions[[hurdle_link]]
    if (is.null(reverse_link_count) || is.null(reverse_link_hurdle)) {
      stop("Unsupported link function in one of the model components.")
    }
    
    # Extract fixed intercept and slope samples for count component
    fixed_intercept_samples_count <- posterior_samples$b_Intercept
    fixed_slope_samples_count <- posterior_samples[[paste0("b_", e)]]
    
    # Extract fixed intercept and slope samples for hurdle component
    fixed_intercept_samples_hurdle <- posterior_samples[[paste0("b_", hurdle_component, "_Intercept")]]
    fixed_slope_samples_hurdle <- posterior_samples[[paste0("b_", hurdle_component, "_", e)]]
    
    # Handle cases where the effect is not in the fixed effects
    if (is.null(fixed_slope_samples_count)) {
      stop(paste("Effect", e, "not found in fixed effects for count component."))
    }
    if (is.null(fixed_slope_samples_hurdle)) {
      # If the effect is not in the hurdle component, set slope to zero
      fixed_slope_samples_hurdle <- rep(0, length(fixed_intercept_samples_hurdle))
    }
    
    # Choose mean or median based on the 'robust' argument
    if (robust) {
      # Use median for robust summaries
      fixed_intercept_central_count <- median(fixed_intercept_samples_count)
      fixed_slope_central_count <- median(fixed_slope_samples_count)
      fixed_intercept_central_hurdle <- median(fixed_intercept_samples_hurdle)
      fixed_slope_central_hurdle <- median(fixed_slope_samples_hurdle)
      if (grepl("lognormal", model$family$family)) {
        sigma_central <- median(sigma_samples)
      }
    } else {
      # Use mean for traditional summaries
      fixed_intercept_central_count <- mean(fixed_intercept_samples_count)
      fixed_slope_central_count <- mean(fixed_slope_samples_count)
      fixed_intercept_central_hurdle <- mean(fixed_intercept_samples_hurdle)
      fixed_slope_central_hurdle <- mean(fixed_slope_samples_hurdle)
      if (grepl("lognormal", model$family$family)) {
        sigma_central <- mean(sigma_samples)
      }
    }
    
    # Generate linear predictors
    linear_predictor_count <- fixed_intercept_central_count + fixed_slope_central_count * predictor_range
    linear_predictor_hurdle <- fixed_intercept_central_hurdle + fixed_slope_central_hurdle * predictor_range
    
    # Apply inverse link functions
    if (grepl("lognormal", model$family$family)) {
      mu_count <- exp(linear_predictor_count)
      mu_count_expected <- exp(linear_predictor_count + 0.5 * sigma_central^2)
    } else {
      mu_count <- reverse_link_count(linear_predictor_count)
      mu_count_expected <- mu_count
    }
    prob_zero <- reverse_link_hurdle(linear_predictor_hurdle)  # P(Y = 0)
    prob_positive <- 1 - prob_zero  # P(Y > 0)
    
    # Expected value
    expected_value <- prob_positive * mu_count_expected
    
    # Apply custom transformation if provided
    if (!is.null(transform_fn)) {
      mu_count <- transform_fn(mu_count)
      mu_count_expected <- transform_fn(mu_count_expected)
      prob_zero <- transform_fn(prob_zero)
      prob_positive <- transform_fn(prob_positive)
      expected_value <- transform_fn(expected_value)
    }
    
    # Create data frames for each component
    fixed_predictions_count <- data.frame(
      x_value = predictor_range,
      response = mu_count
    )
    fixed_predictions_hurdle <- data.frame(
      x_value = predictor_range,
      response = prob_positive
    )
    fixed_predictions_combined <- data.frame(
      x_value = predictor_range,
      response = expected_value
    )
    
    # Calculate credible intervals for each component
    ci_bounds_count <- sapply(predictor_range, function(x_val) {
      lp <- fixed_intercept_samples_count + fixed_slope_samples_count * x_val
      if (grepl("lognormal", model$family$family)) {
        response_samples <- exp(lp)
      } else {
        response_samples <- reverse_link_count(lp)
      }
      if (!is.null(transform_fn)) {
        response_samples <- transform_fn(response_samples)
      }
      response_samples
    })
    fixed_predictions_count$lower <- apply(ci_bounds_count, 2, quantile, probs = 0.025)
    fixed_predictions_count$upper <- apply(ci_bounds_count, 2, quantile, probs = 0.975)
    
    ci_bounds_hurdle <- sapply(predictor_range, function(x_val) {
      lp_hurdle <- fixed_intercept_samples_hurdle + fixed_slope_samples_hurdle * x_val
      prob_zero_samples <- reverse_link_hurdle(lp_hurdle)
      prob_positive_samples <- 1 - prob_zero_samples
      if (!is.null(transform_fn)) {
        prob_positive_samples <- transform_fn(prob_positive_samples)
      }
      prob_positive_samples
    })
    fixed_predictions_hurdle$lower <- apply(ci_bounds_hurdle, 2, quantile, probs = 0.025)
    fixed_predictions_hurdle$upper <- apply(ci_bounds_hurdle, 2, quantile, probs = 0.975)
    
    ci_bounds_combined <- sapply(predictor_range, function(x_val) {
      lp_count <- fixed_intercept_samples_count + fixed_slope_samples_count * x_val
      lp_hurdle <- fixed_intercept_samples_hurdle + fixed_slope_samples_hurdle * x_val
      if (grepl("lognormal", model$family$family)) {
        mu_count_samples <- exp(lp_count + 0.5 * sigma_samples^2)
      } else {
        mu_count_samples <- reverse_link_count(lp_count)
      }
      prob_zero_samples <- reverse_link_hurdle(lp_hurdle)
      prob_positive_samples <- 1 - prob_zero_samples
      expected_value_samples <- prob_positive_samples * mu_count_samples
      if (!is.null(transform_fn)) {
        expected_value_samples <- transform_fn(expected_value_samples)
      }
      expected_value_samples
    })
    fixed_predictions_combined$lower <- apply(ci_bounds_combined, 2, quantile, probs = 0.025)
    fixed_predictions_combined$upper <- apply(ci_bounds_combined, 2, quantile, probs = 0.975)
    
    
    # Set up labels
    ## Plot titles
    if (grepl("lognormal", model$family$family)) {
      positive_component_title <- "Non-Zero Component"
    } else {
      positive_component_title <- "Count Component"
    }
    
    ## Axes Titles
    x_lab <- ifelse(is.null(x_label), e, x_label[[i]])
    single_outcome_name <- ifelse(is.null(y_label), "Y", y_label)
    # Check if y_labels is provided and has the correct length
    if (!is.null(y_labels)) {
      if (length(y_labels) != 3) {
        stop('y_labels must be of length 3. Use y_label to provide only 1')
      }
      # Use y_labels directly if provided
      prob_label <- y_labels[[1]]
      positive_component_ylabel <- y_labels[[2]]
      combined_ylabel <- y_labels[[3]]
    } else {
      # Create labels using single_outcome_name
      prob_label <- if (use_pr_notation) {
        bquote(Pr(.(single_outcome_name) ~ ">" ~ 0))
      } else {
        bquote(P(.(single_outcome_name) ~ ">" ~ 0))
      }
      positive_component_ylabel <- bquote(E*""[.(single_outcome_name) ~ "|" ~ .(single_outcome_name) ~ ">" ~ 0])
      combined_ylabel <- bquote(E*""[.(single_outcome_name)])
    }
    
    
    
    # Create plots for each component
    p_hurdle <- ggplot() +
      geom_ribbon(
        data = fixed_predictions_hurdle,
        aes(x = x_value, ymin = lower, ymax = upper),
        fill = "goldenrod", alpha = 0.24
      ) +
      geom_line(
        data = fixed_predictions_hurdle,
        aes(x = x_value, y = response),
        color = "goldenrod",
        linewidth =1.8
      ) +
      labs(
        title = "Hurdle Component",
        x = x_lab,
        y = prob_label
      ) + ylim(0, 1) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, l = 10)),
        axis.text = element_text(size = 10.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
        panel.border = element_blank(),         # Removes the main plot panel border
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.5, color = 'grey45')
      )
    
    p_count <- ggplot() +
      geom_ribbon(
        data = fixed_predictions_count,
        aes(x = x_value, ymin = lower, ymax = upper),
        fill = "#009E73", alpha = 0.24
      ) +
      geom_line(
        data = fixed_predictions_count,
        aes(x = x_value, y = response),
        color = "#009E73",
        linewidth =1.8
      ) +
      labs(
        title = positive_component_title,
        x = x_lab,
        y = positive_component_ylabel
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, l = 10)),
        axis.text = element_text(size = 10.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
        panel.border = element_blank(),         # Removes the main plot panel border
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.5, color = 'grey45')
      )
    
    p_combined <- ggplot() +
      geom_ribbon(
        data = fixed_predictions_combined,
        aes(x = x_value, ymin = lower, ymax = upper),
        fill = "#6a3d9a", alpha = 0.24
      ) +
      geom_line(
        data = fixed_predictions_combined,
        aes(x = x_value, y = response),
        color = "#6a3d9a",
        linewidth =1.8
      ) +
      labs(
        title = "Combined Expected Value",
        x = x_lab,
        y = combined_ylabel
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(t = 10, b = 10)),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10, l = 10)),
        axis.text = element_text(size = 10.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"),
        panel.border = element_blank(),         # Removes the main plot panel border
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey80", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(linewidth = 0.5, color = 'grey45')
      )
    
    # Include the random effects code
    if (!is.null(group_var)) {
      # Extract random effects for count component
      random_effects_prefix_count <- paste0("r_", group_var)
      random_effects_pattern_count <- paste0("r_", group_var, "\\[(.*),(.+)]")
      random_effects_count <- posterior_samples %>%
        select(.draw, starts_with(random_effects_prefix_count)) %>%
        select(-matches(paste0("__", hurdle_component))) %>%  # Exclude hurdle component random effects
        pivot_longer(
          cols = -c(.draw),
          names_to = c("group_level", ".value"),
          names_pattern = random_effects_pattern_count
        ) %>%
        mutate(group_level = as.character(group_level))
      
      # Random effects for hurdle component
      random_effects_prefix_hurdle <- paste0("r_", group_var, "__", hurdle_component)
      random_effects_pattern_hurdle <- paste0("r_", group_var, "__", hurdle_component, "\\[(.*),(.+)]")
      random_effects_hurdle <- posterior_samples %>%
        select(.draw, starts_with(random_effects_prefix_hurdle)) %>%
        pivot_longer(
          cols = -c(.draw),
          names_to = c("group_level", ".value"),
          names_pattern = random_effects_pattern_hurdle
        ) %>%
        mutate(group_level = as.character(group_level))
      
      # Adjust column names
      names(random_effects_count)[-(1:2)] <- paste0(names(random_effects_count)[-(1:2)], "_count")
      names(random_effects_hurdle)[-(1:2)] <- paste0(names(random_effects_hurdle)[-(1:2)], "_hurdle")
      
      # Merge random effects
      random_effects <- full_join(random_effects_count, random_effects_hurdle, by = c("group_level", ".draw"))
      
      # Handle missing random effects
      random_slope_name <- e
      if (!paste0(random_slope_name, "_count") %in% names(random_effects)) {
        random_effects[[paste0(random_slope_name, "_count")]] <- 0
      }
      if (!paste0(random_slope_name, "_hurdle") %in% names(random_effects)) {
        random_effects[[paste0(random_slope_name, "_hurdle")]] <- 0
      }
      if (!"Intercept_count" %in% names(random_effects)) {
        random_effects$Intercept_count <- 0
      }
      if (!"Intercept_hurdle" %in% names(random_effects)) {
        random_effects$Intercept_hurdle <- 0
      }
      
      # Select groups to include
      unique_groups <- unique(random_effects$group_level)
      if (!is.null(n_groups)) {
        if (n_groups < length(unique_groups)) {
          if (!is.null(seed)) {
            set.seed(seed)  # For reproducibility
          }
          selected_groups <- sample(unique_groups, n_groups)
        } else {
          selected_groups <- unique_groups
        }
      } else {
        selected_groups <- unique_groups
      }
      
      # Filter random effects for selected groups
      random_effects <- random_effects %>% filter(group_level %in% selected_groups)
      
      # Generate individual-level predictions
      individual_predictions <- do.call(rbind, lapply(selected_groups, function(j) {
        # Filter for the posterior samples of this specific group
        individual_samples <- random_effects %>% filter(group_level == j)
        
        # Compute the combined intercept and slope for this group
        if (robust) {
          # Use median for robust summaries
          individual_intercept_count <- fixed_intercept_central_count + median(individual_samples$Intercept_count, na.rm = TRUE)
          individual_slope_count <- fixed_slope_central_count + median(individual_samples[[paste0(e, "_count")]], na.rm = TRUE)
          individual_intercept_hurdle <- fixed_intercept_central_hurdle + median(individual_samples$Intercept_hurdle, na.rm = TRUE)
          individual_slope_hurdle <- fixed_slope_central_hurdle + median(individual_samples[[paste0(e, "_hurdle")]], na.rm = TRUE)
        } else {
          # Use mean for traditional summaries
          individual_intercept_count <- fixed_intercept_central_count + mean(individual_samples$Intercept_count, na.rm = TRUE)
          individual_slope_count <- fixed_slope_central_count + mean(individual_samples[[paste0(e, "_count")]], na.rm = TRUE)
          individual_intercept_hurdle <- fixed_intercept_central_hurdle + mean(individual_samples$Intercept_hurdle, na.rm = TRUE)
          individual_slope_hurdle <- fixed_slope_central_hurdle + mean(individual_samples[[paste0(e, "_hurdle")]], na.rm = TRUE)
        }
        
        # Determine predictor range for this group
        if (plot_full_range) {
          # Use the full predictor range
          predictor_range_group <- predictor_range
        } else {
          # Get the data for this group
          group_data <- model$data[model$data[[group_var]] == j, ]
          
          # Check if there is data for this group
          if (nrow(group_data) == 0) {
            return(NULL)
          }
          
          # Get the range of predictor 'e' for this group
          min_e <- min(group_data[[e]], na.rm = TRUE)
          max_e <- max(group_data[[e]], na.rm = TRUE)
          
          # Handle case where min and max are the same
          if (min_e == max_e) {
            predictor_range_group <- min_e
          } else {
            predictor_range_group <- seq(min_e, max_e, length.out = 100)
          }
        }
        
        # Generate individual-level predictions
        # For count component
        lp_count <- individual_intercept_count + individual_slope_count * predictor_range_group
        if (grepl("lognormal", model$family$family)) {
          mu_count <- exp(lp_count)
          mu_count_expected <- exp(lp_count + 0.5 * sigma_central^2)
        } else {
          mu_count <- reverse_link_count(lp_count)
          mu_count_expected <- mu_count
        }
        
        # For hurdle component
        lp_hurdle <- individual_intercept_hurdle + individual_slope_hurdle * predictor_range_group
        prob_zero <- reverse_link_hurdle(lp_hurdle)
        prob_positive <- 1 - prob_zero
        
        # Expected value
        expected_value <- prob_positive * mu_count_expected
        
        # Apply custom transformation if provided
        if (!is.null(transform_fn)) {
          mu_count <- transform_fn(mu_count)
          prob_positive <- transform_fn(prob_positive)
          expected_value <- transform_fn(expected_value)
        }
        
        # Return data frames for each component
        data.frame(
          group_level = j,
          x_value = predictor_range_group,
          mu_count = mu_count,
          prob_positive = prob_positive,
          expected_value = expected_value
        )
      }))
      
      # Remove any NULL elements
      individual_predictions <- individual_predictions %>% filter(!is.na(expected_value))
      
      # Add individual lines to the plots
      # For hurdle component
      p_hurdle <- p_hurdle +
        geom_line(
          data = individual_predictions,
          aes(x = x_value, y = prob_positive, group = group_level),
          color = "goldenrod",
          linewidth = 0.25,
          alpha = 0.4
        )
      
      # For positive outcome component
      p_count <- p_count +
        geom_line(
          data = individual_predictions,
          aes(x = x_value, y = mu_count, group = group_level),
          color = "#009E73",
          linewidth = 0.25,
          alpha = 0.4
        )
      
      # For combined expected value
      p_combined <- p_combined +
        geom_line(
          data = individual_predictions,
          aes(x = x_value, y = expected_value, group = group_level),
          color = "#6a3d9a",
          linewidth = 0.25,
          alpha = 0.4
        )
    }  # End of random effects code
    
    # Apply x and y limits if specified
    if (!is.null(x_limits)) {
      p_count <- p_count + xlim(x_limits) 
      p_hurdle <- p_hurdle + xlim(x_limits)
      p_combined <- p_combined + xlim(x_limits)
    }
    if (!is.null(y_limits)) {
      p_count <- p_count + ylim(y_limits)
      p_combined <- p_combined + ylim(y_limits)
    }
    
    
    
    ## create posterior density plot
    # Create additional density plot for transformed slopes
    # Prepare the posterior density plot using the current effect
    draws_df <- posterior_samples %>%
      as_draws_df() %>%
      mutate(
        combined_Intercept = (1 - plogis(posterior_samples[[paste0("b_", hurdle_component, "_Intercept")]])) * exp(posterior_samples$b_Intercept),
        predictions = 
          (1 - plogis(posterior_samples[[paste0("b_", hurdle_component, "_Intercept")]] + posterior_samples[[paste0("b_", hurdle_component, "_", e)]])) *
          exp(posterior_samples$b_Intercept + posterior_samples[[paste0("b_", e)]]),
        combined_slope = predictions / combined_Intercept,
        hurdle_slope = 1 / exp(posterior_samples[[paste0("b_", hurdle_component, "_", e)]]),
        nonzero_slope = exp(posterior_samples[[paste0("b_", e)]])
      ) %>%
      select(c(hurdle_slope, nonzero_slope, combined_slope))
    
    # Prepare the data for the current effect
    plot_data <- draws_df %>%
      pivot_longer(cols = c(hurdle_slope, nonzero_slope, combined_slope),
                   names_to = "slope_type",
                   values_to = "value") %>%
      mutate(
        slope_type = factor(
          slope_type, 
          levels = c("combined_slope", "nonzero_slope", "hurdle_slope"),
          labels = c("Combined", "Non-Zero Component", "Hurdle Component")
        )
      )
    
    # Calculate the quantiles for each slope type
    if (!is.null(filter_quantiles)) {
      quantiles <- plot_data %>%
        group_by(slope_type) %>%
        summarize(
          lower = quantile(value, 1 - filter_quantiles),
          upper = quantile(value, filter_quantiles)
        )
      
      # Filter the data to include only values within the 99.99% quantile range
      plot_data <- plot_data %>%
        left_join(quantiles, by = "slope_type") %>%
        filter(value >= lower & value <= upper) %>%
        select(-lower, -upper)
    }
    
    # Create the vertical density plot
    p_density <- ggplot(plot_data, aes(x = value, y = slope_type, height = after_stat(density))) +
      geom_density_ridges_gradient(
        color = 'black',
        linewidth = 0.25,
        aes(fill = after_stat(x > 1)),
        scale = 5,
        rel_min_height = 0.0001,
        gradient_lwd = 0.1,
        quantile_lines = TRUE, 
        quantiles = c(0.025, 0.5, 0.975)  # Adding lines for the median
      ) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
      scale_x_continuous(
        breaks = function(x) unique(c(1, pretty(x)))  # Ensure 1 is included in the breaks
      ) +
      scale_fill_manual(
        values = c("lightcoral", "steelblue2"),
        name = "Effect Direction",
        labels = c("Negative (<1)", "Positive (>1)")
      ) + 
      theme_ridges(font_size = 12, grid = TRUE) +  # Reduced font size for a cleaner look
      theme(
        panel.grid.minor = element_blank(),  # Remove major grid lines for less clutter
        panel.grid.major.x = element_line(color = "grey80", linetype = "dotted"),  # Keep only minor grid lines on x-axis
        panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
        axis.title.x = element_text(hjust = 0.5, size = 12, margin = margin(t = 10)),  # Slightly smaller x-axis title
        axis.text.x = element_text(size = 10.5),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(t = 10, b = 10)),  # Slightly smaller plot title
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10.5),
        legend.position = 'right',  
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),  # Remove legend background for a cleaner look
        legend.key.size = unit(0.7, "cm")
      ) +
      labs(
        x = "Possible Values of Transformed Slopes",
        y = NULL,
        title = "Posterior Density of Fixed Effects",
        subtitle = "Transformed to Represent Multiplicative Changes in Odds Ratios or Expected Values"
      )
    
    
    
    # Arrange plots for vertical option
    design <- "
      AAACCCCCCC
      AAACCCCCCC
      AAACCCCCCC
      BBBCCCCCCC
      BBBCCCCCCC
      BBBCCCCCCC
      DDDDDDDDDD
      DDDDDDDDDD
      DDDDDDDDDD
      DDDDDDDDDD
    "
    
    if (!is.null(p_title)) {
      final_p_title <- p_title
    } else {
      final_p_title <- paste('The Relationship Between', x_lab, 'and', single_outcome_name)
    }
    
    combined_plot <- 
      p_hurdle + p_count + p_combined + free(p_density) + 
      plot_layout(design = design, widths = 1) +
      plot_annotation(
        title = final_p_title,
        subtitle = 'Bayesian Hurdle-Lognormal Model Components: Fixed and Random Effects',
        caption = 'By Pascal Küng',
        theme = theme(
          plot.title = element_text(hjust = 0.5, size = 25, face = "bold", margin = margin(t = 20, b = 15)),
          plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", margin = margin(b = 20))
        )
      ) & theme(
        title = element_text(family = font_family),
        text = element_text(family = font_family) # Adjust size as needed
      )
    
    # Store the combined plot
    plots_list[[e]] <- combined_plot
  }  # End of effects loop
  
  return(plots_list)
}


# Updated function for cumulative models with random effects
plot_cumulative_model <- function(
    model,
    effects,
    group_var,
    n_groups,
    seed,
    robust,
    plot_full_range,
    x_limits,
    y_limits,
    x_label,
    y_label,
    transform_fn
) {
  plots_list <- list()
  
  # Extract posterior samples
  posterior_samples <- posterior::as_draws_df(model) %>% as.data.frame()
  
  # Extract threshold names
  threshold_names <- grep('^b_Intercept\\[', names(posterior_samples), value = TRUE)
  thresholds_samples <- posterior_samples[, threshold_names]
  thresholds_samples <- as.matrix(thresholds_samples)
  
  # Number of categories (K)
  K <- length(threshold_names) + 1
  
  # Define inverse link functions
  inverse_link_functions <- list(
    logit = plogis,
    probit = pnorm,
    cloglog = function(x) 1 - exp(-exp(x)),
    cauchit = function(x) 0.5 + atan(x)/pi,
    identity = function(x) x
  )
  reverse_link <- inverse_link_functions[[model$family$link]]
  if (is.null(reverse_link)) {
    stop("Link function not recognized or not supported.")
  }
  
  for (i in seq_along(effects)) {
    e <- effects[[i]]
    
    # Define a range of values for the predictor across all data
    predictor_range <- seq(min(model$data[[e]], na.rm = TRUE), max(model$data[[e]], na.rm = TRUE), length.out = 100)
    
    # Set up labels
    x_lab <- ifelse(is.null(x_label), e, x_label[[i]])
    y_lab <- ifelse(is.null(y_label), 'Probability Y > 0', y_label)
    
    # Extract fixed slope samples
    beta_samples <- posterior_samples[[paste0("b_", e)]]
    
    # Handle cases where the effect is not in the fixed effects
    if (is.null(beta_samples)) {
      stop(paste("Effect", e, "not found in fixed effects."))
    }
    
    n_samples <- nrow(thresholds_samples)
    
    # Initialize list to store probabilities for each category
    probabilities <- vector("list", K)
    for (k in 1:K) {
      probabilities[[k]] <- data.frame(
        x_value = predictor_range,
        median = NA,
        lower = NA,
        upper = NA
      )
    }
    
    # Loop over predictor values for fixed effects
    for (xi in seq_along(predictor_range)) {
      x_val <- predictor_range[xi]
      
      # Compute linear predictor for all samples
      eta_s <- beta_samples * x_val
      
      # Compute cumulative probabilities for each threshold
      cdf_s <- matrix(NA, nrow = n_samples, ncol = K + 1)
      cdf_s[, 1] <- 0  # P(Y <= 0) = 0
      cdf_s[, K + 1] <- 1  # P(Y <= K) = 1
      
      for (k in 1:(K - 1)) {
        threshold_k_s <- thresholds_samples[, k]
        cdf_s[, k + 1] <- reverse_link(threshold_k_s - eta_s)
      }
      
      # Compute category probabilities
      for (k in 1:K) {
        P_Yk_s <- cdf_s[, k + 1] - cdf_s[, k]
        if (!is.null(transform_fn)) {
          P_Yk_s <- transform_fn(P_Yk_s)
        }
        # Summarize across samples
        if (robust) {
          median_PYk <- median(P_Yk_s)
        } else {
          median_PYk <- mean(P_Yk_s)
        }
        lower_PYk <- quantile(P_Yk_s, probs = 0.025)
        upper_PYk <- quantile(P_Yk_s, probs = 0.975)
        
        # Store in probabilities list
        probabilities[[k]]$median[xi] <- median_PYk
        probabilities[[k]]$lower[xi] <- lower_PYk
        probabilities[[k]]$upper[xi] <- upper_PYk
      }
    }
    
    # Combine data for plotting fixed effects
    plot_data <- do.call(rbind, lapply(1:K, function(k) {
      data.frame(
        x_value = probabilities[[k]]$x_value,
        median = probabilities[[k]]$median,
        lower = probabilities[[k]]$lower,
        upper = probabilities[[k]]$upper,
        category = factor(k)
      )
    }))
    
    # Get default ggplot colors for categories
    num_categories <- K
    default_colors <- scales::hue_pal()(num_categories)
    
    # Initialize ggplot
    p <- ggplot(plot_data, aes(x = x_value, y = median, color = category, fill = category)) +
      # Add CI ribbon
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.24, color = NA) +
      # Add median lines
      geom_line(linewidth =1.8) +
      # Set manual colors for categories
      scale_color_manual(values = default_colors) +
      scale_fill_manual(values = default_colors) +
      labs(
        title = paste("Predicted Probabilities by Category:", x_lab),
        x = x_lab,
        y = y_lab,
        color = "Category",
        fill = "Category"
      ) +
      theme_bw(base_size = 11) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    # If group_var is provided, add random effects
    if (!is.null(group_var)) {
      # Handle random effects
      random_effects_prefix <- paste0("r_", group_var)
      random_effects_pattern <- paste0("r_", group_var, "\\[(.*),(.+)]")
      
      # Extract random effects columns
      random_effects <- posterior_samples %>%
        select(starts_with(random_effects_prefix)) %>%
        pivot_longer(
          cols = everything(),
          names_to = c("group_level", ".value"),
          names_pattern = random_effects_pattern
        ) %>%
        mutate(group_level = as.character(group_level))
      
      # Handle cases where random effects components are not present
      if (!e %in% names(random_effects)) {
        random_effects[[e]] <- 0
      }
      
      # Determine which group levels to include
      unique_groups <- unique(random_effects$group_level)
      if (!is.null(n_groups)) {
        if (n_groups < length(unique_groups)) {
          if (!is.null(seed)) {
            set.seed(seed)  # Set seed for reproducibility
          }
          selected_groups <- sample(unique_groups, n_groups)
        } else {
          selected_groups <- unique_groups
        }
      } else {
        selected_groups <- unique_groups
      }
      
      # Filter random effects to include only selected group levels
      random_effects <- random_effects %>% filter(group_level %in% selected_groups)
      
      # Generate individual-level predictions
      individual_predictions <- do.call(rbind, lapply(selected_groups, function(j) {
        # Filter for the posterior samples of this specific group
        individual_samples <- random_effects %>% filter(group_level == j)
        
        # Get random intercepts for this group
        random_intercepts <- individual_samples$Intercept
        
        # Combine fixed and random effects for slopes
        beta_j_samples <- beta_samples + individual_samples[[e]]
        
        # Determine predictor range for this group
        if (plot_full_range) {
          # Use the full predictor range
          predictor_range_group <- predictor_range
        } else {
          # Get the data for this group
          group_data <- model$data[model$data[[group_var]] == j, ]
          
          # Check if there is data for this group
          if (nrow(group_data) == 0) {
            return(NULL)
          }
          
          # Get the range of predictor 'e' for this group
          min_e <- min(group_data[[e]], na.rm = TRUE)
          max_e <- max(group_data[[e]], na.rm = TRUE)
          
          # Handle case where min and max are the same
          if (min_e == max_e) {
            predictor_range_group <- min_e
          } else {
            predictor_range_group <- seq(min_e, max_e, length.out = 100)
          }
        }
        
        # Initialize list to store probabilities for each category
        probabilities_j <- vector("list", K)
        for (k in 1:K) {
          probabilities_j[[k]] <- data.frame(
            x_value = predictor_range_group,
            median = NA,
            lower = NA,
            upper = NA,
            group_level = j
          )
        }
        
        # Loop over predictor values
        for (xi in seq_along(predictor_range_group)) {
          x_val <- predictor_range_group[xi]
          
          # Compute linear predictor for all samples, now including random intercepts
          eta_s <- beta_j_samples * x_val + random_intercepts
          
          # Compute cumulative probabilities for each threshold
          cdf_s <- matrix(NA, nrow = n_samples, ncol = K + 1)
          cdf_s[, 1] <- 0  # P(Y <= 0) = 0
          cdf_s[, K + 1] <- 1  # P(Y <= K) = 1
          
          for (k in 1:(K - 1)) {
            threshold_k_s <- thresholds_samples[, k]
            cdf_s[, k + 1] <- reverse_link(threshold_k_s - eta_s)
          }
          
          # Compute category probabilities
          for (k in 1:K) {
            P_Yk_s <- cdf_s[, k + 1] - cdf_s[, k]
            if (!is.null(transform_fn)) {
              P_Yk_s <- transform_fn(P_Yk_s)
            }
            # Summarize across samples
            if (robust) {
              median_PYk <- median(P_Yk_s)
            } else {
              median_PYk <- mean(P_Yk_s)
            }
            lower_PYk <- quantile(P_Yk_s, probs = 0.025)
            upper_PYk <- quantile(P_Yk_s, probs = 0.975)
            
            # Store in probabilities list
            probabilities_j[[k]]$median[xi] <- median_PYk
            probabilities_j[[k]]$lower[xi] <- lower_PYk
            probabilities_j[[k]]$upper[xi] <- upper_PYk
          }
        }
        
        # Combine data for plotting
        plot_data_j <- do.call(rbind, lapply(1:K, function(k) {
          data.frame(
            x_value = probabilities_j[[k]]$x_value,
            median = probabilities_j[[k]]$median,
            lower = probabilities_j[[k]]$lower,
            upper = probabilities_j[[k]]$upper,
            category = factor(k),
            group_level = j
          )
        }))
        
        return(plot_data_j)
      }))
      
      # Remove any NULL elements (in case some groups had no data)
      individual_predictions <- individual_predictions %>% filter(!is.na(median))
      
      # Add individual lines to the plot, now using category-specific colors
      p <- p +
        geom_line(
          data = individual_predictions,
          aes(x = x_value, y = median, group = interaction(group_level, category), color = category),
          linewidth = 0.4,
          alpha = 0.4
        ) +
        labs(
          title = paste("Conditional Fixed and Random Effects:", x_lab)
        )
    }
    
    # Apply x and y limits if specified
    if (!is.null(x_limits)) {
      p <- p + xlim(x_limits)
    }
    if (!is.null(y_limits)) {
      p <- p + ylim(y_limits)
    }
    
    # Store the plot
    plots_list[[e]] <- p
  }
  
  return(plots_list)
}



##############################################################################
############################ Check Models ####################################
##############################################################################



pp_check_transformed <- function(model, transform = log1p) {
  # only for univariate models
  outcome_variable <- strsplit(as.character(formula(model)), " ~ ")[[1]][1]
  pred <- brms::posterior_predict(model)
  
  # Use get to dynamically reference the outcome variable in the model's data
  bayesplot::ppc_dens_overlay(
    y = transform(model$data[[outcome_variable]]),
    yrep = transform(pred[1:10, ])
  )
}


# Check brms models for convergence etc. 
check_brms <- function(
    model, 
    log_pp_check = FALSE, # a function needs to be passed!
    transform = log1p,
    pp_type = c('ecdf_overlay', 'dens_overlay'),
    group = NULL
) { 
  print(performance::check_collinearity(model))
  try(print(performance::check_distribution(model)))
  rstan::check_hmc_diagnostics(model$fit)
  plot(model, ask = FALSE, nvariables = 3)
  
  for (type_i in pp_type) {
    plot(pp_check(model, type = type_i, group = group))
  }

  if (log_pp_check) {
    plot(pp_check_transformed(model, transform = transform))
  }
  loo(model)
}
  


DHARMa.check_brms <- function(model,        
                       integer = FALSE,   # integer response? (TRUE/FALSE)
                       plot = TRUE,       
                       ...) {
  
  mdata <- brms::standata(model)
  if (!"Y" %in% names(mdata))
    stop("Cannot extract the required information from this brms model")
  
  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(model, ndraws = 1000)),
    observedResponse = mdata$Y, 
    fittedPredictedResponse = apply(
      t(brms::posterior_epred(model, ndraws = 1000, re.form = NA)),
      1,
      median),
    integerResponse = integer,
    seed = 123
    )
  
  if (isTRUE(plot)) {
    plot(dharma.obj, ...)
  }
  invisible(dharma.obj)
}



DHARMa.check_brms_ordinal <- function(model, plot = TRUE, debug = FALSE, ...) {
  
  # Extract data and posterior predictions
  mdata <- brms::standata(model)
  
  if(debug) {
    cat("Model family:", family(model)$family, "\n")
    cat("Response variable levels:", levels(factor(mdata$Y)), "\n")
  }
  
  # Get posterior predictions
  latent_predictions <- try({
    brms::posterior_epred(model, ndraws = 1000)
  })
  
  if(inherits(latent_predictions, "try-error")) {
    stop("Error in getting posterior predictions. Check if model is ordinal.")
  }
  
  if(debug) {
    cat("Dimensions of latent_predictions:", dim(latent_predictions), "\n")
    cat("Class of latent_predictions:", class(latent_predictions), "\n")
  }
  
  # Convert categorical responses to numeric
  observed_numeric <- as.numeric(factor(mdata$Y))
  
  if(debug) {
    cat("Range of observed_numeric:", range(observed_numeric), "\n")
  }
  
  # For ordinal models, we need to convert the 3D array to a matrix
  # Each row will be a simulation, each column an observation
  sim_response <- try({
    # Convert predictions to probabilities for each category
    probs <- aperm(latent_predictions, c(1, 2, 3))
    # Sample categories based on these probabilities
    samples <- apply(probs, 1:2, function(x) sample(seq_along(x), size = 1, prob = x))
    # Transpose to match DHARMa expectations
    t(samples)
  })
  
  if(inherits(sim_response, "try-error")) {
    stop("Error in processing predictions. Check dimensions.")
  }
  
  if(debug) {
    cat("Dimensions of simulated response:", dim(sim_response), "\n")
  }
  
  # Create fitted predicted response (expected category)
  fitted_pred <- try({
    # Calculate expected category for each observation
    probs_mean <- apply(latent_predictions, 2:3, mean)
    apply(probs_mean, 1, function(x) sum(seq_along(x) * x))
  })
  
  if(inherits(fitted_pred, "try-error")) {
    stop("Error in creating fitted predictions. Check dimensions.")
  }
  
  # Create DHARMa object
  dharma.obj <- try({
    DHARMa::createDHARMa(
      simulatedResponse = sim_response,
      observedResponse = observed_numeric,
      fittedPredictedResponse = fitted_pred,
      integerResponse = TRUE,  # Changed to TRUE for ordinal responses
      seed = 123
    )
  })
  
  if(inherits(dharma.obj, "try-error")) {
    stop("Error in creating DHARMa object. Check all inputs.")
  }
  
  if (isTRUE(plot)) {
    plot(dharma.obj, ...)
  }
  
  invisible(dharma.obj)
}




DHARMa.check_brms.all <- function(model, integer = FALSE, outliers_type = 'default', ...) {
  
  if ("factor" %in% class(model$data[[1]]) 
      & 'ordered' %in% class(model$data[[1]])) {
    model.check <- DHARMa.check_brms_ordinal(model, plot = FALSE, ...)
  } else {
    model.check <- DHARMa.check_brms(model, integer = integer, plot = FALSE, ...)
  }
  
  plot(model.check)
  try(testDispersion(model.check))
  try(testZeroInflation(model.check))
  try(testOutliers(model.check, type = outliers_type))
}






