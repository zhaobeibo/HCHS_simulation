# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

# Set file path
main_path <- "C://Users//zhaob//OneDrive - University of North Carolina at Chapel Hill//CSCC//HCHS//V3_SIM//suddan//HCHS_simulation//"
file_path <- paste0(main_path, "output//alex//GENMOD - GEE Results.xlsx")

threshold <- 0.01

# Read data
method_codes <- read_excel(file_path, sheet = "Method Codes")
batch_output <- read_excel(file_path, sheet = "Batch Output")

# Enhanced function to parse method specifications
create_method_mapping <- function(method_codes) {
  
  method_mapping <- method_codes %>%
    select(suffix, Spec, corr, mitype) %>%
    filter(!is.na(suffix) & !is.na(Spec)) %>%
    mutate(
      # Extract scenario number from Spec
      scenario = str_extract(Spec, "^\\[(\\d+)\\]") %>% str_extract("\\d+") %>% as.numeric(),
      
      # Extract procedure type
      proc_type = case_when(
        str_detect(Spec, "PROC GENMOD") ~ "GENMOD",
        str_detect(Spec, "PROC GEE") ~ "GEE",
        TRUE ~ "UNKNOWN"
      ),
      
      # Extract correlation structure
      correlation = case_when(
        str_detect(Spec, "IND CORR") ~ "Independent",
        str_detect(Spec, "EXCH CORR") ~ "Exchangeable",
        corr == "IND" ~ "Independent", 
        corr == "EXCH" ~ "Exchangeable",
        TRUE ~ "UNKNOWN"
      ),
      
      # Extract missing data method
      missing_method = case_when(
        str_detect(Spec, "\\[MI\\]") ~ "MI",
        str_detect(Spec, "\\[NO MI\\]") ~ "NO MI",
        TRUE ~ "UNKNOWN"
      ),
      
      # Extract weight method
      weight_method = case_when(
        str_detect(Spec, "RR_glm_agestrat") ~ "RR_glm_agestrat_strat",
        str_detect(Spec, "RR_glm_strat") ~ "RR_glm_strat",
        str_detect(Spec, "RR_NRadj_strat") ~ "RR_NRadj_strat", 
        str_detect(Spec, "RR_glm") ~ "RR_glm",
        str_detect(Spec, "RR_NRadj") ~ "RR_NRadj",
        TRUE ~ "UNKNOWN"
      ),
      
      # Extract outcome type
      outcome_type = case_when(
        str_detect(Spec, "BINARY") ~ "Binary",
        mitype == "bin" ~ "Binary",
        mitype == "cont" ~ "Continuous",
        TRUE ~ "Continuous"
      ),
      
      # Extract visit restriction
      visit_restriction = case_when(
        str_detect(Spec, "NOMISS V3") ~ "V3_restricted",
        str_detect(Spec, "bghhsub_s2_v3_nr") ~ "V3_restricted", 
        str_detect(Spec, "bghhsub_s2_nr") ~ "visit_specific",
        TRUE ~ "full_sample"
      ),
      
      # Create scenario description
      scenario_desc = case_when(
        scenario == 1 ~ "MI + V1 Weight (Full Sample)",
        scenario == 2 ~ "No MI + V1 Weight (Full Sample)", 
        scenario == 3 ~ "MI + V3 Adjusted Weight (Restricted Sample)",
        scenario == 4 ~ "No MI + V3 Adjusted Weight (Restricted Sample)",
        scenario == 5 ~ "MI + Visit-specific Weights (Full Sample)",
        scenario == 6 ~ "No MI + Visit-specific Weights (Full Sample)",
        TRUE ~ paste("Scenario", scenario)
      )
    )
  
  return(method_mapping)
}

# Function to prepare data for efficiency comparison
prepare_efficiency_data <- function(batch_output, method_mapping) {
  
  # Handle multiple rows per suffix-parameter combination
  batch_output_with_id <- batch_output %>%
    group_by(suffix, Parm) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  
  # Join with method mapping
  efficiency_data <- batch_output_with_id %>%
    left_join(method_mapping, by = "suffix", relationship = "many-to-many") %>%
    filter(!is.na(scenario))
  
  # Match rows to correlations based on position
  efficiency_data <- efficiency_data %>%
    arrange(suffix, Parm, row_id, correlation) %>%
    group_by(suffix, Parm) %>%
    mutate(
      final_correlation = case_when(
        row_id == 1 ~ "Exchangeable", 
        row_id == 2 ~ "Independent",
        TRUE ~ correlation
      )
    ) %>%
    ungroup() %>%
    filter(correlation == final_correlation) %>%
    select(-row_id, -final_correlation)
  
  return(efficiency_data)
}

# Simplified function to calculate relative SE differences
calculate_se_differences <- function(efficiency_data) {
  
  cat("Starting SE difference calculations...\n")
  
  # Get available procedures dynamically
  available_procs <- unique(efficiency_data$proc_type)
  cat("Available procedures:", paste(available_procs, collapse = ", "), "\n")
  
  # Check what correlation indicators we actually have
  cat("Available group values (correlation indicators):", paste(unique(efficiency_data$group), collapse = ", "), "\n")
  cat("Available correlation values:", paste(unique(efficiency_data$correlation), collapse = ", "), "\n")
  
  # Define comparison sets
  comparison_sets <- list(
    "Set1" = list(
      reference = "RR_glm",
      comparisons = c("RR_NRadj")
    ),
    "Set2" = list(
      reference = "RR_glm_strat", 
      comparisons = c("RR_glm_agestrat_strat", "RR_NRadj_strat")
    )
  )
  
  all_comparisons <- list()
  comparison_count <- 0
  
  # Loop through each comparison set
  for (set_name in names(comparison_sets)) {
    ref_method <- comparison_sets[[set_name]]$reference
    comp_methods <- comparison_sets[[set_name]]$comparisons
    
    cat("\nProcessing comparison set:", set_name, "- Reference method:", ref_method, "\n")
    
    # Loop through available combinations - use 'group' column for correlation
    for (outcome in c("Continuous", "Binary")) {
      for (corr_group in c("IND", "EXCH")) {  # Use actual values from 'group' column
        for (proc in available_procs) {
          
          # Create readable correlation name for output
          corr_name <- ifelse(corr_group == "IND", "Independent", "Exchangeable")
          
          # Get reference data - filter by 'group' column instead of 'correlation'
          ref_data <- efficiency_data %>%
            filter(outcome_type == outcome, 
                   group == corr_group,  # Use 'group' column
                   proc_type == proc,
                   weight_method == ref_method) %>%
            select(scenario, Parm, `Empirical SE`) %>%
            rename(ref_se = `Empirical SE`)
          
          if (nrow(ref_data) == 0) {
            cat("  No reference data for:", outcome, corr_name, proc, ref_method, "\n")
            next
          } else {
            cat("  Found", nrow(ref_data), "reference rows for:", outcome, corr_name, proc, ref_method, "\n")
          }
          
          # Compare each method to reference
          for (comp_method in comp_methods) {
            comp_data <- efficiency_data %>%
              filter(outcome_type == outcome,
                     group == corr_group,  # Use 'group' column
                     proc_type == proc,
                     weight_method == comp_method) %>%
              select(scenario, Parm, `Empirical SE`) %>%
              rename(comp_se = `Empirical SE`)
            
            if (nrow(comp_data) == 0) {
              cat("    No comparison data for:", outcome, corr_name, proc, comp_method, "\n")
              next
            } else {
              cat("    Found", nrow(comp_data), "comparison rows for:", outcome, corr_name, proc, comp_method, "\n")
            }
            
            # Calculate relative differences
            comparison_result <- ref_data %>%
              inner_join(comp_data, by = c("scenario", "Parm")) %>%
              mutate(
                relative_se_diff = (comp_se - ref_se) / ref_se,
                relative_se_diff_pct = relative_se_diff * 100,
                efficiency_issue = ifelse(abs(relative_se_diff) > threshold, 1, 0),
                comparison_set = set_name,
                outcome_type = outcome,
                correlation = corr_name,  # Use readable name for output
                proc_type = proc,
                reference_method = ref_method,
                comparison_method = comp_method
              ) %>%
              # Add verification columns for debugging
              mutate(
                manual_calc = (comp_se - ref_se) / ref_se * 100,
                diff_check = abs(relative_se_diff_pct - manual_calc)
              )
            
            if (nrow(comparison_result) > 0) {
              
              # Debug: Show a few sample calculations
              if (comparison_count == 0) {  # Only show for first comparison
                cat("      Debug - Sample calculations:\n")
                debug_sample <- comparison_result %>% 
                  head(3) %>%
                  select(Parm, ref_se, comp_se, relative_se_diff_pct, manual_calc, diff_check)
                print(debug_sample)
              }
              
              comparison_key <- paste(set_name, outcome, corr_name, proc, comp_method, sep = "_")
              all_comparisons[[comparison_key]] <- comparison_result
              comparison_count <- comparison_count + 1
              cat("      ✓ Successfully created comparison:", comparison_key, "with", nrow(comparison_result), "rows\n")
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

# Function to create summary tables for efficiency issues
create_efficiency_summary <- function(se_comparisons) {
  
  if (nrow(se_comparisons) == 0) {
    return(data.frame())
  }
  
  # Create summary by comparison set, scenario, outcome, correlation, and procedure
  efficiency_summary <- se_comparisons %>%
    group_by(comparison_set, scenario, outcome_type, correlation, proc_type, 
             reference_method, comparison_method) %>%
    summarise(
      total_comparisons = n(),
      efficiency_issues = sum(efficiency_issue, na.rm = TRUE),
      mean_rel_se_diff_pct = mean(relative_se_diff_pct, na.rm = TRUE),
      median_rel_se_diff_pct = median(relative_se_diff_pct, na.rm = TRUE),
      max_abs_rel_se_diff_pct = max(abs(relative_se_diff_pct), na.rm = TRUE),
      min_rel_se_diff_pct = min(relative_se_diff_pct, na.rm = TRUE),
      max_rel_se_diff_pct = max(relative_se_diff_pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      efficiency_issue_rate = efficiency_issues / total_comparisons,
      comparison_label = paste0(comparison_method, " vs ", reference_method)
    )
  
  return(efficiency_summary)
}

# Function to create detailed efficiency comparison tables
create_detailed_efficiency_tables <- function(se_comparisons) {
  
  if (nrow(se_comparisons) == 0) {
    return(list())
  }
  
  tables <- list()
  
  # Create tables for each comparison set and scenario
  for (set_name in unique(se_comparisons$comparison_set)) {
    for (scenario_num in unique(se_comparisons$scenario)) {
      
      set_scenario_data <- se_comparisons %>%
        filter(comparison_set == set_name, scenario == scenario_num)
      
      # Create separate tables for each outcome and procedure combination
      for (outcome in unique(set_scenario_data$outcome_type)) {
        for (proc in unique(set_scenario_data$proc_type)) {
          
          table_name <- paste0(set_name, "_S", scenario_num, "_", outcome, "_", proc, "_Efficiency")
          
          filtered_data <- set_scenario_data %>%
            filter(outcome_type == outcome, proc_type == proc) %>%
            select(scenario, Parm, correlation, reference_method, comparison_method,
                   ref_se, comp_se, relative_se_diff_pct, efficiency_issue) %>%
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

# Function to create comprehensive efficiency summary
create_comprehensive_efficiency_summary <- function(efficiency_summary) {
  
  if (nrow(efficiency_summary) == 0) {
    return(list())
  }
  
  summary_tables <- list()
  
  # Create separate tables for each comparison set
  for (set_name in unique(efficiency_summary$comparison_set)) {
    
    set_data <- efficiency_summary %>%
      filter(comparison_set == set_name)
    
    # Create a wide table for this comparison set only
    set_table <- set_data %>%
      select(scenario, outcome_type, proc_type, correlation, 
             comparison_label, efficiency_issues) %>%
      unite("method_combo", outcome_type, proc_type, correlation, comparison_label, sep = "_") %>%
      select(scenario, method_combo, efficiency_issues) %>%
      pivot_wider(
        names_from = method_combo,
        values_from = efficiency_issues,
        values_fill = 0
      ) %>%
      arrange(scenario)
    
    summary_tables[[paste0(set_name, "_Summary")]] <- set_table
  }
  
  return(summary_tables)
}

# Function to create scenario-specific summary tables (also separated by comparison set)
create_scenario_summary_tables <- function(efficiency_summary) {
  
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
      
      # Create a wide table for this scenario and comparison set
      scenario_table <- scenario_set_data %>%
        select(outcome_type, proc_type, correlation, 
               comparison_label, efficiency_issues) %>%
        unite("method_combo", outcome_type, proc_type, correlation, comparison_label, sep = "_") %>%
        select(method_combo, efficiency_issues) %>%
        pivot_wider(
          names_from = method_combo,
          values_from = efficiency_issues,
          values_fill = 0
        )
      
      table_name <- paste0("S", scenario_num, "_", set_name, "_Summary")
      scenario_tables[[table_name]] <- scenario_table
    }
  }
  
  return(scenario_tables)
}

# Main execution
cat("=== Starting Efficiency Analysis ===\n")

# Create method mapping
cat("Creating method mapping...\n")
method_mapping <- create_method_mapping(method_codes)

# Debug: Check method mapping
cat("Method mapping created with", nrow(method_mapping), "rows\n")
cat("Unique weight methods found:", paste(unique(method_mapping$weight_method), collapse = ", "), "\n")

# Prepare efficiency data
cat("Preparing efficiency data...\n")
efficiency_data <- prepare_efficiency_data(batch_output, method_mapping)

# Debug: Check efficiency data
cat("Efficiency data prepared with", nrow(efficiency_data), "rows\n")

# Calculate SE differences
cat("Calculating SE differences...\n")
se_comparisons <- calculate_se_differences(efficiency_data)

# ADDITIONAL DEBUGGING - Let's see what's actually in the efficiency_data
cat("\n=== DETAILED DEBUGGING ===\n")
cat("Let's examine the efficiency_data structure in detail...\n")

# Check a few specific combinations to see what's going wrong
debug_check <- efficiency_data %>%
  filter(outcome_type == "Continuous", proc_type == "GENMOD", weight_method == "RR_glm") %>%
  select(suffix, scenario, Parm, correlation, `Empirical SE`) %>%
  head(10)

cat("Sample of RR_glm data for Continuous outcome:\n")
print(debug_check)

# Check correlation values
cat("\nUnique correlation values in efficiency_data:\n")
print(unique(efficiency_data$correlation))

# Check if there's data for each combination
combinations_check <- efficiency_data %>%
  group_by(outcome_type, correlation, proc_type, weight_method) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(weight_method %in% c("RR_glm", "RR_glm_strat"))

cat("\nAvailable combinations for reference methods:\n")
print(combinations_check)

# Let's try a manual filter to see what we get
manual_filter <- efficiency_data %>%
  filter(outcome_type == "Continuous" & 
           correlation == "Independent" & 
           proc_type == "GENMOD" & 
           weight_method == "RR_glm")

cat("\nManual filter for Continuous + Independent + GENMOD + RR_glm:\n")
cat("Rows found:", nrow(manual_filter), "\n")

if (nrow(manual_filter) > 0) {
  cat("Sample data:\n")
  print(head(manual_filter %>% select(suffix, scenario, Parm, `Empirical SE`)))
}

# Create output based on whether we have comparisons
if (nrow(se_comparisons) > 0) {
  
  # Create summary tables
  cat("Creating efficiency summaries...\n")
  efficiency_summary <- create_efficiency_summary(se_comparisons)
  
  # Create detailed tables
  cat("Creating detailed efficiency tables...\n")
  detailed_efficiency_tables <- create_detailed_efficiency_tables(se_comparisons)
  
  # Create comprehensive summary
  cat("Creating comprehensive efficiency summary...\n")
  comprehensive_efficiency_summary <- create_comprehensive_efficiency_summary(efficiency_summary)
  
  # Create scenario-specific summary tables
  cat("Creating scenario-specific summary tables...\n")
  scenario_summary_tables <- create_scenario_summary_tables(efficiency_summary)
  
  # Create Excel output
  output_file <- paste0(main_path, "output//Efficiency_Comparison_Results.xlsx")
  wb <- createWorkbook()
  
  # Add comparison set summaries (Set1 and Set2 separate)
  for (summary_name in names(comprehensive_efficiency_summary)) {
    sheet_name <- substr(summary_name, 1, 31) # Excel sheet name limit
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, comprehensive_efficiency_summary[[summary_name]])
  }
  
  # Add scenario-specific summary tables (separated by comparison set)
  for (scenario_name in names(scenario_summary_tables)) {
    sheet_name <- substr(scenario_name, 1, 31) # Excel sheet name limit
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, scenario_summary_tables[[scenario_name]])
  }
  
  # Add efficiency summary table
  addWorksheet(wb, "Efficiency_Summary_Stats")
  writeData(wb, "Efficiency_Summary_Stats", efficiency_summary)
  
  # Add detailed comparison tables
  for (table_name in names(detailed_efficiency_tables)) {
    sheet_name <- substr(table_name, 1, 31) # Excel sheet name limit
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, detailed_efficiency_tables[[table_name]])
  }
  
  # Add raw comparison data for reference
  addWorksheet(wb, "Raw_SE_Comparisons")
  writeData(wb, "Raw_SE_Comparisons", se_comparisons)
  
  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("=== Analysis Complete ===\n")
  cat("Efficiency comparison results saved to:", output_file, "\n")
  
  # Display summary information
  cat("\n=== SUMMARY OF EFFICIENCY ANALYSIS ===\n")
  cat("Excel file contains multiple sheets:\n")
  
  # List comparison set summaries
  if (length(comprehensive_efficiency_summary) > 0) {
    for (i in seq_along(comprehensive_efficiency_summary)) {
      cat(paste0(i, ". ", names(comprehensive_efficiency_summary)[i], " - Efficiency issues for ", names(comprehensive_efficiency_summary)[i], "\n"))
    }
  }
  
  # List scenario-specific sheets
  if (length(scenario_summary_tables) > 0) {
    start_num <- length(comprehensive_efficiency_summary) + 1
    for (i in seq_along(scenario_summary_tables)) {
      cat(paste0(start_num + i - 1, ". ", names(scenario_summary_tables)[i], " - Specific scenario and comparison set\n"))
    }
  }
  
  summary_sheets <- length(comprehensive_efficiency_summary) + length(scenario_summary_tables)
  cat(paste0(summary_sheets + 1, ". Efficiency_Summary_Stats - Detailed statistics for each comparison\n"))
  
  if (length(detailed_efficiency_tables) > 0) {
    cat(paste0(summary_sheets + 2, " onwards: Detailed efficiency comparison tables (by scenario/outcome/procedure)\n"))
  }
  
  cat("Final sheet: Raw_SE_Comparisons - Raw comparison data\n")
  
  if (length(detailed_efficiency_tables) > 0) {
    for (i in seq_along(detailed_efficiency_tables)) {
      cat(paste0(i+2, ". ", names(detailed_efficiency_tables)[i], " - Detailed efficiency comparison\n"))
    }
  }
  
  cat(paste0(length(detailed_efficiency_tables)+3, ". Raw_SE_Comparisons - Raw comparison data\n"))
  
  # Show preview of key results
  cat("\n=== PREVIEW OF COMPREHENSIVE EFFICIENCY SUMMARY ===\n")
  print(comprehensive_efficiency_summary)
  
  cat("\n=== PREVIEW OF EFFICIENCY SUMMARY STATS ===\n")
  print(efficiency_summary)
  
  # Show some key statistics
  cat("\n=== KEY EFFICIENCY STATISTICS ===\n")
  
  # Count total efficiency issues by comparison set and scenario
  total_issues <- se_comparisons %>%
    group_by(comparison_set, scenario) %>%
    summarise(
      total_efficiency_issues = sum(efficiency_issue, na.rm = TRUE),
      total_comparisons = n(),
      issue_rate = total_efficiency_issues / total_comparisons,
      .groups = "drop"
    )
  
  cat("Total efficiency issues by comparison set and scenario:\n")
  print(total_issues)
  
  # Show worst efficiency issues
  worst_issues <- se_comparisons %>%
    filter(efficiency_issue == 1) %>%
    arrange(desc(abs(relative_se_diff_pct))) %>%
    head(10) %>%
    select(comparison_set, outcome_type, correlation, proc_type, 
           scenario, Parm, comparison_method, relative_se_diff_pct)
  
  if (nrow(worst_issues) > 0) {
    cat("\nTop 10 worst efficiency issues (>% SE difference):\n")
    print(worst_issues)
  } else {
    cat("\nNo efficiency issues found (no cases with >% SE difference)\n")
  }
  
  cat("\n=== Analysis completed successfully! ===\n")
  
} else {
  cat("ERROR: No comparisons could be created. Please check the data structure.\n")
  
  # Show some debugging info
  cat("\nDebugging information:\n")
  cat("- Efficiency data rows:", nrow(efficiency_data), "\n")
  cat("- Available procedures:", paste(unique(efficiency_data$proc_type), collapse = ", "), "\n")
  cat("- Available weight methods:", paste(unique(efficiency_data$weight_method), collapse = ", "), "\n")
  cat("- Available outcome types:", paste(unique(efficiency_data$outcome_type), collapse = ", "), "\n")
  cat("- Available correlations:", paste(unique(efficiency_data$correlation), collapse = ", "), "\n")
}