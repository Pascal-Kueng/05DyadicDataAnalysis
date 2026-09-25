# Main Functions in this file (3):

###################################
# 1. summarize_brms
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
# - exponentiate_sigma: A logical value indicating whether to exponentiate brms sigma
#   distributional parameters while leaving the other coefficients unchanged. Default is FALSE.
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
# 2. summarize_exchangeable_apim_brms
###################################
# Description:
# This function rotates the common/couple-mean and member-deviation random-effect blocks from an
# exchangeable-dyad brms model back to the full APIM partner-level
# variance-covariance matrix. It performs the rotation for every posterior draw,
# so SDs, correlations, covariances, fixed effects, and residual quantities can
# be reported with posterior intervals on the transformed scale.

###################################
# 3. summarize_distinguishable_apim_brms
###################################
# Description:
# This function summarizes fixed effects, group-level SDs/correlations, and
# residual quantities from a distinguishable-dyad brms APIM. Sigma parameters are
# converted from the brms log scale to residual SDs before residual variances and
# same-day residual covariances are computed.


###########################################################################
####################### REPORT MODELS #####################################
###########################################################################


summarize_brms <- function(model, 
                           exponentiate = FALSE,
                           exponentiate_sigma = FALSE,
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
  
  # brms estimates sigma distributional parameters on the link scale.
  if (exponentiate_sigma && !is.null(random_effects)) {
    sigma_rows <- grepl("^(b_)?sigma_", rownames(random_effects))
    if (any(sigma_rows)) {
      random_effects$Estimate[sigma_rows] <- exp(random_effects$Estimate[sigma_rows])
      random_effects$`u-95% CI`[sigma_rows] <- exp(random_effects$`u-95% CI`[sigma_rows])
      random_effects$`l-95% CI`[sigma_rows] <- exp(random_effects$`l-95% CI`[sigma_rows])
      random_effects$SE[sigma_rows] <- random_effects$Estimate[sigma_rows] * random_effects$SE[sigma_rows]
    }
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



##############################################################################
############################ Check Models ####################################
##############################################################################

  


DHARMa.check_brms <- function(model,        
                       integer = FALSE,   # integer response? (TRUE/FALSE)
                       ndraws = 1000,
                       plot = TRUE,       
                       ...) {
  
  mdata <- brms::standata(model)
  if (!"Y" %in% names(mdata))
    stop("Cannot extract the required information from this brms model")
  
  observed_response <- as.vector(mdata$Y)
  
  simulated_response <- brms::posterior_predict(model, ndraws = ndraws)
  if (length(dim(simulated_response)) != 2) {
    stop("DHARMa.check_brms currently supports only univariate brms models")
  }
  if (ncol(simulated_response) == length(observed_response)) {
    simulated_response <- t(simulated_response)
  } else if (nrow(simulated_response) != length(observed_response)) {
    stop(
      "Dimension mismatch: posterior_predict returned ",
      paste(dim(simulated_response), collapse = " x "),
      " values, but observed response has length ",
      length(observed_response)
    )
  }
  
  fitted_response <- brms::posterior_epred(model, ndraws = ndraws)
  if (length(dim(fitted_response)) == 2 && ncol(fitted_response) == length(observed_response)) {
    fitted_response <- apply(fitted_response, 2, median)
  } else if (length(dim(fitted_response)) == 2 && nrow(fitted_response) == length(observed_response)) {
    fitted_response <- apply(fitted_response, 1, median)
  } else {
    fitted_response <- apply(simulated_response, 1, median)
  }
  
  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = simulated_response,
    observedResponse = observed_response, 
    fittedPredictedResponse = fitted_response,
    integerResponse = integer,
    seed = 123
    )
  
  if (isTRUE(plot)) {
    plot(dharma.obj, ...)
  }
  invisible(dharma.obj)
}



summarise_posterior_vector <- function(x, probs = c(0.025, 0.975)) {
  tibble::tibble(
    estimate = mean(x),
    est_error = stats::sd(x),
    q_low = stats::quantile(x, probs[1], names = FALSE),
    q_high = stats::quantile(x, probs[2], names = FALSE)
  )
}


make_brms_covariance_draw <- function(draw, terms, gr = "coupleID") {
  p <- length(terms)
  sds <- as.numeric(draw[paste0("sd_", gr, "__", terms)])
  cor_mat <- diag(1, p)

  if (anyNA(sds)) {
    missing_sd <- paste0("sd_", gr, "__", terms)[is.na(sds)]
    stop("Missing brms SD parameter(s): ", paste(missing_sd, collapse = ", "))
  }

  if (p > 1) {
    for (j in 2:p) {
      for (i in 1:(j - 1)) {
        cor_name <- paste0("cor_", gr, "__", terms[i], "__", terms[j])
        cor_name_rev <- paste0("cor_", gr, "__", terms[j], "__", terms[i])

        if (cor_name %in% names(draw)) {
          cor_mat[i, j] <- cor_mat[j, i] <- as.numeric(draw[[cor_name]])
        } else if (cor_name_rev %in% names(draw)) {
          cor_mat[i, j] <- cor_mat[j, i] <- as.numeric(draw[[cor_name_rev]])
        } else {
          stop("Missing brms correlation parameter: ", cor_name)
        }
      }
    }
  }

  diag(sds) %*% cor_mat %*% diag(sds)
}


summarize_exchangeable_apim_brms <- function(
    fit,
    gr = "coupleID",
    deviation_term = ".member_contrast_arbitrary",
    term_labels = NULL,
    partner_labels = c("A", "B"),
    probs = c(0.025, 0.975),
    include_fixed = TRUE,
    include_residual = TRUE
) {
  if (length(partner_labels) != 2) {
    stop("partner_labels must contain exactly two labels.")
  }

  random_effects <- fit$ranef
  random_effects <- random_effects[random_effects$group == gr, ]

  is_deviation <- grepl(deviation_term, random_effects$coef, fixed = TRUE)
  common_terms <- random_effects$coef[!is_deviation]
  deviation_terms <- random_effects$coef[is_deviation]

  if (length(common_terms) != length(deviation_terms)) {
    stop("The common/couple-mean and member-deviation random-effect blocks must contain the same number of terms.")
  }

  if (is.null(term_labels)) {
    term_labels <- common_terms
  }

  if (length(term_labels) != length(common_terms)) {
    stop("term_labels must have the same length as the common/couple-mean random-effect terms.")
  }

  draws <- as.data.frame(posterior::as_draws_df(fit))

  covariance_draws <- purrr::map(seq_len(nrow(draws)), function(i) {
    sigma_common <- make_brms_covariance_draw(draws[i, ], common_terms, gr = gr)
    sigma_deviation <- make_brms_covariance_draw(draws[i, ], deviation_terms, gr = gr)

    within_person <- sigma_common + sigma_deviation
    cross_person <- sigma_common - sigma_deviation

    full <- rbind(
      cbind(within_person, cross_person),
      cbind(cross_person, within_person)
    )

    labels <- c(
      paste0(partner_labels[1], "_", term_labels),
      paste0(partner_labels[2], "_", term_labels)
    )

    dimnames(within_person) <- list(term_labels, term_labels)
    dimnames(cross_person) <- list(term_labels, term_labels)
    dimnames(full) <- list(labels, labels)

    list(
      within_person = within_person,
      cross_person = cross_person,
      full_covariance = full
    )
  })

  mean_matrix <- function(component) {
    Reduce("+", purrr::map(covariance_draws, component)) / length(covariance_draws)
  }

  full_covariance_matrix <- mean_matrix("full_covariance")
  within_person_covariance_matrix <- mean_matrix("within_person")
  cross_person_covariance_matrix <- mean_matrix("cross_person")
  full_correlation_matrix <- Reduce("+", purrr::map(covariance_draws, ~ stats::cov2cor(.x$full_covariance))) / length(covariance_draws)

  covariance_summary <- purrr::map_dfr(covariance_draws, function(draw) {
    as.data.frame(as.table(draw$full_covariance)) |>
      dplyr::rename(parameter_1 = Var1, parameter_2 = Var2, value = Freq) |>
      dplyr::mutate(parameter = paste(parameter_1, "with", parameter_2)) |>
      dplyr::select(parameter, value)
  }, .id = "draw") |>
    dplyr::group_by(parameter) |>
    dplyr::summarise(summarise_posterior_vector(value, probs = probs), .groups = "drop")

  sd_summary <- purrr::map_dfr(covariance_draws, function(draw) {
    sds <- sqrt(diag(draw$full_covariance))
    tibble::tibble(parameter = names(sds), value = unname(sds))
  }, .id = "draw") |>
    dplyr::group_by(parameter) |>
    dplyr::summarise(summarise_posterior_vector(value, probs = probs), .groups = "drop")

  cor_summary <- purrr::map_dfr(covariance_draws, function(draw) {
    cor_matrix <- stats::cov2cor(draw$full_covariance)
    as.data.frame(as.table(cor_matrix)) |>
      dplyr::rename(parameter_1 = Var1, parameter_2 = Var2, value = Freq) |>
      dplyr::filter(as.integer(parameter_1) > as.integer(parameter_2)) |>
      dplyr::mutate(parameter = paste(parameter_1, "with", parameter_2)) |>
      dplyr::select(parameter, value)
  }, .id = "draw") |>
    dplyr::group_by(parameter) |>
    dplyr::summarise(summarise_posterior_vector(value, probs = probs), .groups = "drop")

  fixed_summary <- NULL
  if (include_fixed) {
    fixed_summary <- brms::fixef(fit) |>
      as.data.frame() |>
      tibble::rownames_to_column("parameter") |>
      dplyr::transmute(
        section = "Fixed effects",
        parameter,
        estimate = Estimate,
        est_error = Est.Error,
        q_low = Q2.5,
        q_high = Q97.5
      )
  }

  residual_summary <- NULL
  if (include_residual) {
    # brms sigma regression coefficients (b_sigma_*) are on the log scale.
    # In models without a sigma formula, posterior$sigma is already the
    # residual SD on the response scale.
    if ("sigma" %in% names(draws)) {
      sigma_response <- draws$sigma
    } else if ("b_sigma_Intercept" %in% names(draws)) {
      sigma_response <- exp(draws$b_sigma_Intercept)
    } else {
      sigma_response <- NULL
    }

    residual_draws <- tibble::tibble()

    if (!is.null(sigma_response)) {
      residual_draws <- tibble::tibble(
        sigma = sigma_response,
        residual_variance = sigma_response^2
      )
    }

    cortime_name <- grep("^cortime__", names(draws), value = TRUE)
    if (length(cortime_name) > 1L) {
      stop("Expected at most one residual correlation parameter in this two-member model.")
    }

    if (length(cortime_name) == 1L) {
      residual_correlation <- draws[[cortime_name]]
      residual_draws$same_day_residual_correlation <- residual_correlation

      if (!is.null(sigma_response)) {
        residual_draws$same_day_residual_covariance <- residual_correlation * sigma_response^2
      }
    }

    if (ncol(residual_draws) > 0) {
      residual_summary <- residual_draws |>
        tidyr::pivot_longer(
          dplyr::everything(),
          names_to = "parameter",
          values_to = "value"
        ) |>
        dplyr::group_by(parameter) |>
        dplyr::summarise(summarise_posterior_vector(value, probs = probs), .groups = "drop") |>
        dplyr::mutate(section = "Residual structure", .before = parameter)
    }
  }

  reporting_summary <- dplyr::bind_rows(
    fixed_summary,
    sd_summary |>
      dplyr::mutate(section = "Back-transformed random-effect SDs", .before = parameter),
    cor_summary |>
      dplyr::mutate(section = "Back-transformed random-effect correlations", .before = parameter),
    residual_summary
  )

  list(
    within_person_covariance_matrix = within_person_covariance_matrix,
    cross_person_covariance_matrix = cross_person_covariance_matrix,
    full_covariance_matrix = full_covariance_matrix,
    full_correlation_matrix = full_correlation_matrix,
    covariance_summary = covariance_summary,
    sd_summary = sd_summary,
    cor_summary = cor_summary,
    residual_summary = residual_summary,
    fixed_summary = fixed_summary,
    reporting_summary = reporting_summary,
    covariance_draws = covariance_draws
  )
}


summarize_distinguishable_apim_brms <- function(
    fit,
    gr = "coupleID",
    probs = c(0.025, 0.975),
    include_fixed = TRUE,
    include_random = TRUE,
    include_residual = TRUE
) {
  draws <- as.data.frame(posterior::as_draws_df(fit))

  fixed_summary <- NULL
  if (include_fixed) {
    fixed_summary <- brms::fixef(fit) |>
      as.data.frame() |>
      tibble::rownames_to_column("parameter") |>
      dplyr::filter(!grepl("^sigma_", parameter)) |>
      dplyr::transmute(
        section = "Fixed effects",
        parameter,
        estimate = Estimate,
        est_error = Est.Error,
        q_low = Q2.5,
        q_high = Q97.5
      )
  }

  random_sd_summary <- NULL
  random_cor_summary <- NULL
  if (include_random) {
    sd_prefix <- paste0("sd_", gr, "__")
    sd_names <- grep(paste0("^", sd_prefix), names(draws), value = TRUE)

    if (length(sd_names) > 0) {
      random_sd_summary <- draws |>
        dplyr::select(dplyr::all_of(sd_names)) |>
        tidyr::pivot_longer(
          dplyr::everything(),
          names_to = "parameter",
          values_to = "value"
        ) |>
        dplyr::mutate(parameter = sub(paste0("^", sd_prefix), "", parameter)) |>
        dplyr::group_by(parameter) |>
        dplyr::summarise(summarise_posterior_vector(value, probs = probs), .groups = "drop")
    }

    cor_prefix <- paste0("cor_", gr, "__")
    cor_names <- grep(paste0("^", cor_prefix), names(draws), value = TRUE)

    if (length(cor_names) > 0) {
      random_cor_summary <- draws |>
        dplyr::select(dplyr::all_of(cor_names)) |>
        tidyr::pivot_longer(
          dplyr::everything(),
          names_to = "parameter",
          values_to = "value"
        ) |>
        dplyr::mutate(
          parameter = sub(paste0("^", cor_prefix), "", parameter),
          parameter = gsub("__", " with ", parameter)
        ) |>
        dplyr::group_by(parameter) |>
        dplyr::summarise(summarise_posterior_vector(value, probs = probs), .groups = "drop")
    }
  }

  residual_summary <- NULL
  if (include_residual) {
    sigma_names <- grep("^b_sigma_", names(draws), value = TRUE)
    residual_draws <- tibble::tibble()

    if (length(sigma_names) > 0) {
      sigma_draws <- exp(draws[, sigma_names, drop = FALSE])
      names(sigma_draws) <- sub("^b_sigma_", "sigma_", sigma_names)

      residual_draws <- dplyr::bind_cols(
        as_tibble(sigma_draws),
        as_tibble(sigma_draws^2) |>
          dplyr::rename_with(~ sub("^sigma_", "residual_variance_", .x))
      )
    } else if ("sigma" %in% names(draws)) {
      residual_draws <- tibble::tibble(
        sigma = draws$sigma,
        residual_variance = draws$sigma^2
      )
    }

    cortime_name <- grep("^cortime__", names(draws), value = TRUE)
    if (length(cortime_name) > 1L) {
      stop("Expected at most one residual correlation parameter in this two-member model.")
    }

    if (length(cortime_name) == 1L) {
      residual_correlation <- draws[[cortime_name]]
      residual_draws$same_day_residual_correlation <- residual_correlation

      if (all(c("sigma_.is_male", "sigma_.is_female") %in% names(residual_draws))) {
        residual_draws$same_day_residual_covariance <-
          residual_correlation *
          residual_draws[["sigma_.is_male"]] *
          residual_draws[["sigma_.is_female"]]
      } else if ("sigma" %in% names(residual_draws)) {
        residual_draws$same_day_residual_covariance <-
          residual_correlation * residual_draws$sigma^2
      }
    }

    if (ncol(residual_draws) > 0) {
      residual_summary <- residual_draws |>
        tidyr::pivot_longer(
          dplyr::everything(),
          names_to = "parameter",
          values_to = "value"
        ) |>
        dplyr::group_by(parameter) |>
        dplyr::summarise(summarise_posterior_vector(value, probs = probs), .groups = "drop") |>
        dplyr::mutate(section = "Residual structure", .before = parameter)
    }
  }

  random_sd_report <- NULL
  if (!is.null(random_sd_summary)) {
    random_sd_report <- random_sd_summary |>
      dplyr::mutate(section = "Random-effect SDs", .before = parameter)
  }

  random_cor_report <- NULL
  if (!is.null(random_cor_summary)) {
    random_cor_report <- random_cor_summary |>
      dplyr::mutate(section = "Random-effect correlations", .before = parameter)
  }

  reporting_summary <- dplyr::bind_rows(
    fixed_summary,
    random_sd_report,
    random_cor_report,
    residual_summary
  )

  list(
    fixed_summary = fixed_summary,
    random_sd_summary = random_sd_summary,
    random_cor_summary = random_cor_summary,
    residual_summary = residual_summary,
    reporting_summary = reporting_summary
  )
}
