# Load required libraries
library(haven)        # For reading SAS datasets
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

# Set file paths
main_path <- "J://HCHS//STATISTICS//GRAS//Beibo//Computing Requests//HCHS_simulation//"
sas_data_path <- paste0(main_path, "data//derived//summary//")  # Adjust path to where SAS datasets are stored

# Set threshold for efficiency issues
threshold <- 0.01

# Function to create method mapping based on dataset naming conventions
create_method_mapping_sas <- function() {
  
  # Create mapping for all combinations
  method_mapping <- data.frame(
    stringsAsFactors = FALSE,
    
    # Continuous outcomes
    # S1 - MI scenarios
    input_lib = c(rep("s1", 4), rep("s2", 2), rep("s3", 4), rep("s4", 4), rep("s5", 4), rep("s6", 4)),
    corr = c("ind", "exch", "ind", "exch", 
             "ind", "exch", 
             "ind", "exch", "ind", "exch",
             "ind", "exch", "ind", "exch", 
             "ind", "exch", "ind", "exch", 
             "ind", "exch", "ind", "exch"),
    rr = c("_rr_glm_", "_rr_glm_", "_rr_nradj_", "_rr_nradj_", 
           "_", "_",
           "_rr_glm_", "_rr_glm_", "_rr_nradj_", "_rr_nradj_",
           "_rr_glm_", "_rr_glm_", "_rr_nradj_", "_rr_nradj_",
           "_rr_glm_", "_rr_glm_", "_rr_nradj_", "_rr_nradj_",
           "_rr_glm_", "_rr_glm_", "_rr_nradj_", "_rr_nradj_"),
    outcome_type = rep("Continuous",22),
    proc_type = rep("SUDAAN", 22),
    scenario = c(1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6),
    correlation = rep(c("Independent", "Exchangeable"), 11),
    weight_method = c("RR_glm", "RR_glm", "RR_NRadj", "RR_NRadj", "RR_glm", "RR_glm",
                      "RR_glm", "RR_glm", "RR_NRadj", "RR_NRadj",
                      "RR_glm", "RR_glm", "RR_NRadj", "RR_NRadj",
                      "RR_glm", "RR_glm", "RR_NRadj", "RR_NRadj",
                      "RR_glm", "RR_glm", "RR_NRadj", "RR_NRadj"),
    missing_method = c(rep("MI", 4), rep("NO MI", 2), rep("MI", 4), rep("NO MI", 4), rep("MI", 4), rep("NO MI", 4))
  )
  
  # Add binary outcomes (bs1, bs2, bs3, bs4, bs5, bs6)
  binary_mapping <- method_mapping
  binary_mapping$input_lib <- paste0("b", binary_mapping$input_lib)
  binary_mapping$outcome_type <- "Binary"
  
  # Combine continuous and binary
  all_mapping <- rbind(method_mapping, binary_mapping)
  
  # Add scenario descriptions
  all_mapping$scenario_desc <- case_when(
    all_mapping$scenario == 1 ~ "MI + V1 Weight (Full Sample)",
    all_mapping$scenario == 2 ~ "No MI + V1 Weight (Full Sample)",
    all_mapping$scenario == 3 ~ "MI + V3 Adjusted Weight (Restricted Sample)",
    all_mapping$scenario == 4 ~ "No MI + V3 Adjusted Weight (Restricted Sample)",
    all_mapping$scenario == 5 ~ "MI + Visit-specific Weights (Full Sample)",
    all_mapping$scenario == 6 ~ "No MI + Visit-specific Weights (Full Sample)",
    TRUE ~ paste("Scenario", all_mapping$scenario)
  )
  
  return(all_mapping)
}

# Function to read SAS datasets and combine results
read_sas_results <- function(method_mapping, sas_data_path) {
  
  all_results <- data.frame()
  
  for (i in 1:nrow(method_mapping)) {
    row <- method_mapping[i, ]
    
    # Construct dataset name based on naming convention from combine files
    dataset_name <- paste0(row$input_lib, "_", row$corr, row$rr)
    dataset_path <- paste0(sas_data_path, dataset_name, ".sas7bdat")
    
    cat("Attempting to read:", dataset_path, "\n")
    
    # Check if file exists
    if (file.exists(dataset_path)) {
      tryCatch({
        # Read SAS dataset
        sas_data <- read_sas(dataset_path)
        
        # Add method information
        sas_data$input_lib <- row$input_lib
        sas_data$corr <- row$corr
        sas_data$rr <- row$rr
        sas_data$outcome_type <- row$outcome_type
        sas_data$proc_type <- row$proc_type
        sas_data$scenario <- row$scenario
        sas_data$scenario_desc <- row$scenario_desc
        sas_data$correlation <- row$correlation
        sas_data$weight_method <- row$weight_method
        sas_data$missing_method <- row$missing_method
        
        # Standardize column names (SAS datasets might have different cases)
        names(sas_data) <- tolower(names(sas_data))
        
        # Rename columns to match expected format
        if ("parm" %in% names(sas_data)) sas_data$Parm <- sas_data$parm
        if ("estimate" %in% names(sas_data)) sas_data$Estimate <- sas_data$estimate
        if ("empbias" %in% names(sas_data)) sas_data$`Empirical Bias` <- sas_data$empbias
        if ("relbias" %in% names(sas_data)) sas_data$`Relative Bias` <- sas_data$relbias
        if ("empse" %in% names(sas_data)) sas_data$`Empirical SE` <- sas_data$empse
        if ("estse" %in% names(sas_data)) sas_data$`Estimated SE` <- sas_data$estse
        if ("relse" %in% names(sas_data)) sas_data$`Relative SE difference` <- sas_data$relse
        if ("coverage" %in% names(sas_data)) sas_data$Coverage <- sas_data$coverage
        if ("prejecth0" %in% names(sas_data)) sas_data$`P(reject H0)` <- sas_data$prejecth0
        if ("true" %in% names(sas_data)) sas_data$`True Value` <- sas_data$true
        
        # Bind to all results
        all_results <- rbind(all_results, sas_data)
        
        cat("Successfully read:", dataset_name, "with", nrow(sas_data), "rows\n")
        
      }, error = function(e) {
        cat("Error reading", dataset_path, ":", e$message, "\n")
      })
    } else {
      cat("File not found:", dataset_path, "\n")
    }
  }
  
  return(all_results)
}

# Function to calculate SE differences for SUDAAN data
calculate_se_differences_sudaan <- function(efficiency_data) {
  
  cat("Starting SUDAAN SE difference calculations...\n")
  
  # Get available procedures dynamically
  available_procs <- unique(efficiency_data$proc_type)
  cat("Available procedures:", paste(available_procs, collapse = ", "), "\n")
  
  # Define comparison sets based on SUDAAN weight methods
  comparison_sets <- list(
    "Set1" = list(
      reference = "RR_glm",
      comparisons = c("RR_NRadj")
    )
  )
  
  all_comparisons <- list()
  comparison_count <- 0
  
  # Loop through each comparison set
  for (set_name in names(comparison_sets)) {
    ref_method <- comparison_sets[[set_name]]$reference
    comp_methods <- comparison_sets[[set_name]]$comparisons
    
    cat("\nProcessing comparison set:", set_name, "- Reference method:", ref_method, "\n")
    
    # Loop through available combinations
    for (outcome in c("Continuous", "Binary")) {
      for (correlation in c("Independent", "Exchangeable")) {
        for (proc in available_procs) {
          for (scenario in unique(efficiency_data$scenario)) {
            for (missing in unique(efficiency_data$missing_method)) {
              
              # Get reference data
              ref_data <- efficiency_data %>%
                filter(outcome_type == outcome, 
                       correlation == correlation,
                       proc_type == proc,
                       scenario == scenario,
                       missing_method == missing,
                       weight_method == ref_method) %>%
                select(Parm, `Empirical SE`) %>%
                rename(ref_se = `Empirical SE`)
              
              if (nrow(ref_data) == 0) {
                next
              }
              
              # Compare each method to reference
              for (comp_method in comp_methods) {
                comp_data <- efficiency_data %>%
                  filter(outcome_type == outcome,
                         correlation == correlation,
                         proc_type == proc,
                         scenario == scenario,
                         missing_method == missing,
                         weight_method == comp_method) %>%
                  select(Parm, `Empirical SE`) %>%
                  rename(comp_se = `Empirical SE`)
                
                if (nrow(comp_data) == 0) {
                  next
                }
                
                # Calculate relative differences
                comparison_result <- ref_data %>%
                  inner_join(comp_data, by = "Parm") %>%
                  mutate(
                    relative_se_diff = (comp_se - ref_se) / ref_se,
                    relative_se_diff_pct = relative_se_diff * 100,
                    # Create separate efficiency issue indicators for positive and negative
                    efficiency_issue_abs = ifelse(abs(relative_se_diff) > threshold, 1, 0),
                    efficiency_issue_pos = ifelse(relative_se_diff > threshold, 1, 0),
                    efficiency_issue_neg = ifelse(relative_se_diff < -threshold, 1, 0),
                    comparison_set = set_name,
                    outcome_type = outcome,
                    correlation = correlation,
                    proc_type = proc,
                    scenario = scenario,
                    missing_method = missing,
                    reference_method = ref_method,
                    comparison_method = comp_method
                  )
                
                if (nrow(comparison_result) > 0) {
                  comparison_key <- paste(set_name, outcome, correlation, proc, scenario, missing, comp_method, sep = "_")
                  all_comparisons[[comparison_key]] <- comparison_result
                  comparison_count <- comparison_count + 1
                  cat("      ✓ Successfully created comparison:", comparison_key, "with", nrow(comparison_result), "rows\n")
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Combine all comparisons
  if (length(all_comparisons) > 0) {
    combined_comparisons <- bind_rows(all_comparisons)
    cat("\nTotal successful comparisons:", comparison_count, "\n")
    cat("Total combined comparison rows:", nrow(combined_comparisons), "\n")
  } else {
    combined_comparisons <- data.frame()
    cat("\nNo successful comparisons created!\n")
  }
  
  return(combined_comparisons)
}

# Function to create summary tables for SUDAAN efficiency issues with pos/neg counts
create_efficiency_summary_sudaan <- function(se_comparisons) {
  
  if (nrow(se_comparisons) == 0) {
    return(data.frame())
  }
  
  # Create summary by comparison set, scenario, outcome, correlation, procedure, and missing method
  efficiency_summary <- se_comparisons %>%
    group_by(comparison_set, scenario, outcome_type, correlation, proc_type, 
             missing_method, reference_method, comparison_method) %>%
    summarise(
      total_comparisons = n(),
      # Calculate separate positive and negative efficiency issues
      efficiency_issues_pos = sum(efficiency_issue_pos, na.rm = TRUE),
      efficiency_issues_neg = sum(efficiency_issue_neg, na.rm = TRUE),
      efficiency_issues_total = sum(efficiency_issue_abs, na.rm = TRUE),
      mean_rel_se_diff_pct = mean(relative_se_diff_pct, na.rm = TRUE),
      median_rel_se_diff_pct = median(relative_se_diff_pct, na.rm = TRUE),
      max_abs_rel_se_diff_pct = max(abs(relative_se_diff_pct), na.rm = TRUE),
      min_rel_se_diff_pct = min(relative_se_diff_pct, na.rm = TRUE),
      max_rel_se_diff_pct = max(relative_se_diff_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Create formatted efficiency issue display
      efficiency_issues_formatted = case_when(
        efficiency_issues_total == 0 ~ "0",
        efficiency_issues_pos > 0 & efficiency_issues_neg > 0 ~ paste0("+", efficiency_issues_pos, " -", efficiency_issues_neg),
        efficiency_issues_pos > 0 & efficiency_issues_neg == 0 ~ paste0("+", efficiency_issues_pos),
        efficiency_issues_pos == 0 & efficiency_issues_neg > 0 ~ paste0("-", efficiency_issues_neg),
        TRUE ~ as.character(efficiency_issues_total)
      ),
      efficiency_issue_rate = efficiency_issues_total / total_comparisons,
      comparison_label = paste0(comparison_method, " vs ", reference_method)
    )
  
  return(efficiency_summary)
}

# Function to create comprehensive SUDAAN efficiency summary
create_comprehensive_efficiency_summary_sudaan <- function(efficiency_summary) {
  
  if (nrow(efficiency_summary) == 0) {
    return(list())
  }
  
  summary_tables <- list()
  
  # Create separate tables for each comparison set
  for (set_name in unique(efficiency_summary$comparison_set)) {
    
    set_data <- efficiency_summary %>%
      filter(comparison_set == set_name)
    
    # Create a wide table using the formatted efficiency issues
    set_table <- set_data %>%
      select(scenario, outcome_type, proc_type, correlation, missing_method,
             comparison_label, efficiency_issues_formatted) %>%
      unite("method_combo", outcome_type, proc_type, correlation, missing_method, comparison_label, sep = "_") %>%
      select(scenario, method_combo, efficiency_issues_formatted) %>%
      pivot_wider(
        names_from = method_combo,
        values_from = efficiency_issues_formatted,
        values_fill = "0"
      ) %>%
      arrange(scenario)
    
    summary_tables[[paste0(set_name, "_Summary")]] <- set_table
  }
  
  return(summary_tables)
}

# Function to create detailed SUDAAN efficiency comparison tables
create_detailed_efficiency_tables_sudaan <- function(se_comparisons) {
  
  if (nrow(se_comparisons) == 0) {
    return(list())
  }
  
  tables <- list()
  
  # Create tables for each combination
  for (set_name in unique(se_comparisons$comparison_set)) {
    for (scenario_num in unique(se_comparisons$scenario)) {
      for (outcome in unique(se_comparisons$outcome_type)) {
        for (missing in unique(se_comparisons$missing_method)) {
          
          table_name <- paste0(set_name, "_S", scenario_num, "_", outcome, "_", missing, "_Efficiency")
          
          filtered_data <- se_comparisons %>%
            filter(comparison_set == set_name, 
                   scenario == scenario_num,
                   outcome_type == outcome,
                   missing_method == missing) %>%
            select(scenario, Parm, correlation, reference_method, comparison_method,
                   ref_se, comp_se, relative_se_diff_pct, efficiency_issue_pos, efficiency_issue_neg) %>%
            arrange(correlation, comparison_method, Parm)
          
          if (nrow(filtered_data) > 0) {
            tables[[table_name]] <- filtered_data
          }
        }
      }
    }
  }
  
  return(tables)
}

# Function to create scenario-specific summary tables for SUDAAN
create_scenario_summary_tables_sudaan <- function(efficiency_summary) {
  
  if (nrow(efficiency_summary) == 0) {
    return(list())
  }
  
  scenario_tables <- list()
  
  # Create separate summary tables for each scenario and comparison set
  for (scenario_num in unique(efficiency_summary$scenario)) {
    for (set_name in unique(efficiency_summary$comparison_set)) {
      
      scenario_set_data <- efficiency_summary %>%
        filter(scenario == scenario_num, comparison_set == set_name)
      
      if (nrow(scenario_set_data) == 0) next
      
      # Create a wide table using formatted efficiency issues
      scenario_table <- scenario_set_data %>%
        select(outcome_type, correlation, missing_method,
               comparison_label, efficiency_issues_formatted) %>%
        unite("method_combo", outcome_type, correlation, missing_method, comparison_label, sep = "_") %>%
        select(method_combo, efficiency_issues_formatted) %>%
        pivot_wider(
          names_from = method_combo,
          values_from = efficiency_issues_formatted,
          values_fill = "0"
        )
      
      table_name <- paste0("S", scenario_num, "_", set_name, "_Summary")
      scenario_tables[[table_name]] <- scenario_table
    }
  }
  
  return(scenario_tables)
}

# Main execution
cat("=== Starting SUDAAN Efficiency Analysis ===\n")

# Create method mapping
cat("Creating method mapping...\n")
method_mapping <- create_method_mapping_sas()

# Read SUDAAN results
cat("Reading SUDAAN datasets...\n")
sudaan_data <- read_sas_results(method_mapping, sas_data_path)

if (nrow(sudaan_data) > 0) {
  
  # Debug: Check SUDAAN data structure
  cat("SUDAAN data read with", nrow(sudaan_data), "rows\n")
  cat("Unique weight methods found:", paste(unique(sudaan_data$weight_method), collapse = ", "), "\n")
  cat("Unique scenarios found:", paste(unique(sudaan_data$scenario), collapse = ", "), "\n")
  cat("Unique outcome types found:", paste(unique(sudaan_data$outcome_type), collapse = ", "), "\n")
  
  # Calculate SE differences
  cat("Calculating SUDAAN SE differences...\n")
  se_comparisons <- calculate_se_differences_sudaan(sudaan_data)
  
  # Create output based on whether we have comparisons
  if (nrow(se_comparisons) > 0) {
    
    # Create summary tables
    cat("Creating SUDAAN efficiency summaries...\n")
    efficiency_summary <- create_efficiency_summary_sudaan(se_comparisons)
    
    # Create detailed tables
    cat("Creating detailed SUDAAN efficiency tables...\n")
    detailed_efficiency_tables <- create_detailed_efficiency_tables_sudaan(se_comparisons)
    
    # Create comprehensive summary
    cat("Creating comprehensive SUDAAN efficiency summary...\n")
    comprehensive_efficiency_summary <- create_comprehensive_efficiency_summary_sudaan(efficiency_summary)
    
    # Create scenario-specific summary tables
    cat("Creating scenario-specific SUDAAN summary tables...\n")
    scenario_summary_tables <- create_scenario_summary_tables_sudaan(efficiency_summary)
    
    # Create Excel output
    output_file <- paste0(main_path, "output//SUDAAN_Efficiency_Comparison_Results.xlsx")
    wb <- createWorkbook()
    
    # Add comparison set summaries
    for (summary_name in names(comprehensive_efficiency_summary)) {
      sheet_name <- substr(summary_name, 1, 31) # Excel sheet name limit
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, comprehensive_efficiency_summary[[summary_name]])
    }
    
    # Add scenario-specific summary tables
    for (scenario_name in names(scenario_summary_tables)) {
      sheet_name <- substr(scenario_name, 1, 31) # Excel sheet name limit
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, scenario_summary_tables[[scenario_name]])
    }
    
    # Add efficiency summary table
    addWorksheet(wb, "SUDAAN_Efficiency_Summary")
    writeData(wb, "SUDAAN_Efficiency_Summary", efficiency_summary)
    
    # Add detailed comparison tables
    for (table_name in names(detailed_efficiency_tables)) {
      sheet_name <- substr(table_name, 1, 31) # Excel sheet name limit
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, detailed_efficiency_tables[[table_name]])
    }
    
    # Add raw comparison data for reference
    addWorksheet(wb, "Raw_SUDAAN_SE_Comparisons")
    writeData(wb, "Raw_SUDAAN_SE_Comparisons", se_comparisons)
    
    # Add method mapping for reference
    addWorksheet(wb, "Method_Mapping")
    writeData(wb, "Method_Mapping", method_mapping)
    
    # Save the workbook
    saveWorkbook(wb, output_file, overwrite = TRUE)
    
    cat("=== SUDAAN Efficiency Analysis Complete ===\n")
    cat("Results saved to:", output_file, "\n")
    
    # Display summary information
    cat("\n=== SUMMARY OF SUDAAN EFFICIENCY ANALYSIS ===\n")
    cat("Excel file contains efficiency comparisons with positive/negative SE difference counts:\n")
    
    # Show preview of key results
    cat("\n=== PREVIEW OF SUDAAN EFFICIENCY SUMMARY ===\n")
    print(head(efficiency_summary, 10))
    
    # Show some key statistics with positive/negative breakdown
    cat("\n=== KEY SUDAAN EFFICIENCY STATISTICS ===\n")
    
    # Count total efficiency issues by scenario
    total_issues <- se_comparisons %>%
      group_by(scenario, outcome_type, missing_method) %>%
      summarise(
        total_pos_issues = sum(efficiency_issue_pos, na.rm = TRUE),
        total_neg_issues = sum(efficiency_issue_neg, na.rm = TRUE),
        total_abs_issues = sum(efficiency_issue_abs, na.rm = TRUE),
        total_comparisons = n(),
        issue_rate = total_abs_issues / total_comparisons,
        .groups = "drop"
      ) %>%
      mutate(
        formatted_issues = case_when(
          total_abs_issues == 0 ~ "0",
          total_pos_issues > 0 & total_neg_issues > 0 ~ paste0("+", total_pos_issues, " -", total_neg_issues),
          total_pos_issues > 0 & total_neg_issues == 0 ~ paste0("+", total_pos_issues),
          total_pos_issues == 0 & total_neg_issues > 0 ~ paste0("-", total_neg_issues),
          TRUE ~ as.character(total_abs_issues)
        )
      )
    
    cat("SUDAAN efficiency issues by scenario/outcome/missing method:\n")
    print(total_issues %>% select(scenario, outcome_type, missing_method, formatted_issues, issue_rate))
    
    cat("\n=== SUDAAN Analysis completed successfully! ===\n")
    cat("Note: Efficiency issues displayed as:\n")
    cat("- '0' = no efficiency issues\n")
    cat("- '+X' = X parameters where comparison method has LARGER SE (less efficient)\n") 
    cat("- '-X' = X parameters where comparison method has SMALLER SE (more efficient)\n")
    cat("- '+X -Y' = X parameters less efficient and Y parameters more efficient\n")
    cat("# in the cell: # of parameters with SE differences outside the acceptable range\n")
    cat("(+less efficient -more efficient, relative to reference method)\n")
    
  } else {
    cat("ERROR: No SUDAAN comparisons could be created. Please check the data structure.\n")
    
    # Show debugging info
    cat("\nDebugging information:\n")
    cat("- SUDAAN data rows:", nrow(sudaan_data), "\n")
    cat("- Available procedures:", paste(unique(sudaan_data$proc_type), collapse = ", "), "\n")
    cat("- Available weight methods:", paste(unique(sudaan_data$weight_method), collapse = ", "), "\n")
    cat("- Available outcome types:", paste(unique(sudaan_data$outcome_type), collapse = ", "), "\n")
    cat("- Available correlations:", paste(unique(sudaan_data$correlation), collapse = ", "), "\n")
    cat("- Available scenarios:", paste(unique(sudaan_data$scenario), collapse = ", "), "\n")
    cat("- Available missing methods:", paste(unique(sudaan_data$missing_method), collapse = ", "), "\n")
  }
  
} else {
  cat("ERROR: No SUDAAN data was successfully read from SAS datasets.\n")
  cat("Please check:\n")
  cat("1. File paths are correct\n")
  cat("2. SAS datasets exist in the specified directory\n")
  cat("3. Dataset naming convention matches the expected pattern\n")
  
  cat("\n=== EXPECTED DATASET NAMES ===\n")
  expected_names <- paste0(method_mapping$input_lib, "_", method_mapping$corr, method_mapping$rr)
  cat("Looking for these datasets:\n")
  for (name in unique(expected_names)) {
    cat(paste0(name, ".sas7bdat\n"))
  }
}