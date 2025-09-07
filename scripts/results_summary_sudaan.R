# Load required libraries
library(haven)        # For reading SAS datasets
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

# Set file paths
main_path <- "J://HCHS//STATISTICS//GRAS//Beibo//Computing Requests//HCHS_simulation//"
sas_data_path <- paste0(main_path, "data//derived//summary//")  # Adjust path to where SAS datasets are stored

# set threshold for problematic coverage
lower_threshold <- 0.920
upper_threshold <- 0.980


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
    proc_type = rep("GENMOD", 22),
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
        # Keep _FREQ_ column for sample size information
        if ("_freq_" %in% names(sas_data)) sas_data$`_freq_` <- sas_data$`_freq_`
        
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

# Function to create summary table for coverage issues
create_summary_table_sas <- function(batch_output) {
  
  # Calculate coverage issues
  coverage_data <- batch_output %>%
    mutate(
      Coverage_numeric = as.numeric(Coverage),
      coverage_issue = ifelse(Coverage_numeric < lower_threshold | Coverage_numeric > upper_threshold, 1, 0)
    )
  
  # Create summary for each scenario-method combination
  summary_data <- coverage_data %>%
    group_by(scenario, scenario_desc, outcome_type, proc_type, correlation, 
             weight_method, missing_method) %>%
    summarise(
      total_params = n(),
      coverage_issues = sum(coverage_issue, na.rm = TRUE),
      avg_coverage = mean(Coverage_numeric, na.rm = TRUE),
      min_coverage = min(Coverage_numeric, na.rm = TRUE),
      max_coverage = max(Coverage_numeric, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(summary_data)
}

# Function to create comprehensive summary table
create_comprehensive_summary_sas <- function(summary_data) {
  
  # Create a comprehensive table with all combinations
  comprehensive <- summary_data %>%
    select(scenario, scenario_desc, outcome_type, proc_type, correlation, 
           weight_method, coverage_issues) %>%
    # Create a combined identifier for the columns
    unite("method_id", outcome_type, proc_type, correlation, weight_method, sep = "_") %>%
    select(scenario, scenario_desc, method_id, coverage_issues) %>%
    pivot_wider(
      names_from = method_id,
      values_from = coverage_issues,
      values_fill = 0
    ) %>%
    arrange(scenario)
  
  return(comprehensive)
}

# Function to create clean summary tables
create_clean_summary_tables_sas <- function(summary_data) {
  
  # Create separate tables for each outcome type and procedure combination
  tables <- list()
  
  for (outcome in c("Continuous", "Binary")) {
    for (proc in c("GENMOD")) {  # Assuming only GENMOD for now
      
      table_name <- paste(outcome, "Outcome, PROC", proc)
      
      # Filter data for this combination
      filtered_data <- summary_data %>%
        filter(outcome_type == outcome, proc_type == proc)
      
      if (nrow(filtered_data) > 0) {
        # Create the summary table with coverage issues in cells
        summary_table <- filtered_data %>%
          select(scenario, scenario_desc, correlation, weight_method, coverage_issues) %>%
          # Create column names combining correlation and weight method
          unite("column_name", correlation, weight_method, sep = "_") %>%
          select(scenario, scenario_desc, column_name, coverage_issues) %>%
          pivot_wider(
            names_from = column_name,
            values_from = coverage_issues,
            values_fill = 0
          ) %>%
          arrange(scenario)
        
        tables[[table_name]] <- summary_table
      }
    }
  }
  
  return(tables)
}

# Function to create sample size summary
create_sample_size_summary <- function(batch_output) {
  
  # Create sample size summary by extracting _FREQ_ information
  sample_size_summary <- batch_output %>%
    select(scenario, scenario_desc, outcome_type, proc_type, correlation, 
           weight_method, missing_method, input_lib, corr, rr, `_freq_`) %>%
    distinct() %>%
    arrange(scenario, outcome_type, correlation, weight_method)
  
  # Rename _freq_ to Sample_Size for clarity
  if ("_freq_" %in% names(sample_size_summary)) {
    sample_size_summary <- sample_size_summary %>%
      rename(Sample_Size = `_freq_`)
  }
  
  # Create a pivot table showing sample sizes by method combination
  sample_size_pivot <- sample_size_summary %>%
    select(scenario, scenario_desc, outcome_type, correlation, weight_method, Sample_Size) %>%
    unite("method_id", outcome_type, correlation, weight_method, sep = "_") %>%
    select(scenario, scenario_desc, method_id, Sample_Size) %>%
    pivot_wider(
      names_from = method_id,
      values_from = Sample_Size,
      values_fill = NA
    ) %>%
    arrange(scenario)
  
  return(list(
    detailed = sample_size_summary,
    pivot = sample_size_pivot
  ))
}

# Function to create detailed Excel blocks
create_excel_blocks_sas <- function(batch_output) {
  
  blocks_list <- list()
  
  scenarios <- sort(unique(batch_output$scenario))
  correlations <- c("Independent", "Exchangeable")
  missing_methods <- c("MI", "NO MI")
  outcome_types <- c("Continuous", "Binary")
  proc_types <- c("GENMOD")
  
  for (outcome in outcome_types) {
    for (proc in proc_types) {
      for (scenario in scenarios) {
        for (missing in missing_methods) {
          
          # Create block for this combination
          block_name <- paste0("S", scenario, "_", outcome, "_", proc, "_", missing)
          
          # Filter data for this block
          block_data <- batch_output %>%
            filter(scenario == !!scenario, 
                   outcome_type == !!outcome,
                   proc_type == !!proc,
                   missing_method == !!missing) %>%
            arrange(correlation, weight_method, Parm)
          
          if (nrow(block_data) > 0) {
            # Format the block
            formatted_block <- block_data %>%
              select(scenario, Parm, `True Value`, Estimate, `Empirical Bias`, `Relative Bias`,
                     `Empirical SE`, `Estimated SE`, `Relative SE difference`, 
                     Coverage, `P(reject H0)`, correlation, weight_method) %>%
              mutate(
                `Relative Bias` = paste0(round(`Relative Bias` * 100, 1), "%"),
                `Relative SE difference` = paste0(round(`Relative SE difference` * 100, 1), "%"),
                Coverage = round(Coverage, 3),
                `P(reject H0)` = round(`P(reject H0)`, 3),
                # Add coverage issue indicator
                Coverage_Issue = ifelse(Coverage < lower_threshold | Coverage > upper_threshold, "YES", "NO")
              )
            
            blocks_list[[block_name]] <- formatted_block
          }
        }
      }
    }
  }
  
  return(blocks_list)
}

# Main execution
cat("=== Creating method mapping ===\n")
method_mapping <- create_method_mapping_sas()

cat("=== Reading SAS datasets ===\n")
batch_output <- read_sas_results(method_mapping, sas_data_path)

if (nrow(batch_output) > 0) {
  cat("=== Creating summary data ===\n")
  summary_data <- create_summary_table_sas(batch_output)
  
  cat("=== Creating comprehensive summary ===\n")
  comprehensive_summary <- create_comprehensive_summary_sas(summary_data)
  
  cat("=== Creating clean summary tables ===\n")
  clean_summary_tables <- create_clean_summary_tables_sas(summary_data)
  
  cat("=== Creating detailed Excel blocks ===\n")
  excel_blocks <- create_excel_blocks_sas(batch_output)
  
  cat("=== Creating sample size summary ===\n")
  sample_size_summary <- create_sample_size_summary(batch_output)
  
  # Create Excel workbook with results
  output_file <- paste0(main_path, "output//SUDAAN_Results.xlsx")
  wb <- createWorkbook()
  
  # Add comprehensive summary first
  addWorksheet(wb, "Coverage_Issues_Summary")
  writeData(wb, "Coverage_Issues_Summary", comprehensive_summary)
  
  # Add sample size summary sheets
  addWorksheet(wb, "Sample_Size_Summary")
  writeData(wb, "Sample_Size_Summary", sample_size_summary$pivot)
  
  addWorksheet(wb, "Sample_Size_Detailed")
  writeData(wb, "Sample_Size_Detailed", sample_size_summary$detailed)
  
  # Add each clean summary table
  for (table_name in names(clean_summary_tables)) {
    sheet_name <- str_replace_all(table_name, "[^A-Za-z0-9_]", "_")
    sheet_name <- substr(sheet_name, 1, 31) # Excel sheet name limit
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, clean_summary_tables[[table_name]])
  }
  
  # Add detailed blocks
  for (block_name in names(excel_blocks)) {
    sheet_name <- substr(block_name, 1, 31) # Excel sheet name limit
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, excel_blocks[[block_name]])
  }
  
  # Add raw summary data for reference
  addWorksheet(wb, "Raw_Summary_Data")
  writeData(wb, "Raw_Summary_Data", summary_data)
  
  # Add method mapping for reference
  addWorksheet(wb, "Method_Mapping")
  writeData(wb, "Method_Mapping", method_mapping)
  
  # Save the workbook
  saveWorkbook(wb, output_file, overwrite = TRUE)
  cat("SAS results analysis saved to:", output_file, "\n")
  
  # Display summary
  cat("\n=== PREVIEW OF COMPREHENSIVE SUMMARY ===\n")
  print(comprehensive_summary)
  
  cat("\n=== PREVIEW OF SAMPLE SIZE SUMMARY ===\n")
  print(sample_size_summary$pivot)
  
  cat("\n=== SUMMARY STATISTICS ===\n")
  cat("Total datasets processed:", nrow(method_mapping), "\n")
  cat("Successfully read datasets:", length(unique(paste0(batch_output$input_lib, "_", batch_output$corr, batch_output$rr))), "\n")
  cat("Total parameters analyzed:", nrow(batch_output), "\n")
  cat("Total coverage issues found:", sum(summary_data$coverage_issues), "\n")
  
  # Sample size statistics
  if (nrow(sample_size_summary$detailed) > 0) {
    cat("Sample size range:", min(sample_size_summary$detailed$Sample_Size, na.rm = TRUE), 
        "to", max(sample_size_summary$detailed$Sample_Size, na.rm = TRUE), "\n")
  }
  
} else {
  cat("No data was successfully read from SAS datasets. Please check:\n")
  cat("1. File paths are correct\n")
  cat("2. SAS datasets exist in the specified directory\n")
  cat("3. Dataset naming convention matches the expected pattern\n")
}

cat("\n=== EXPECTED DATASET NAMES ===\n")
expected_names <- paste0(method_mapping$input_lib, "_", method_mapping$corr, method_mapping$rr)
cat("Looking for these datasets:\n")
for (name in unique(expected_names)) {
  cat(paste0(name, ".sas7bdat\n"))
}