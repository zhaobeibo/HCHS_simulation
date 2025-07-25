# Load required libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(knitr)

# Set file path (assuming data is already loaded)
main_path <- "J://HCHS//STATISTICS//GRAS//Beibo//Computing Requests//HCHS_simulation//"
file_path <- paste0(main_path, "output//alex//GENMOD - GEE Results.xlsx")
method_codes <- read_excel(file_path, sheet = "Method Codes")
batch_output <- read_excel(file_path, sheet = "Batch Output")

# Function to parse method specifications and create mapping
create_method_mapping <- function(method_codes) {
  
  # Extract relevant information from the Spec column
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
        TRUE ~ "UNKNOWN"
      ),
      
      # Extract missing data method
      missing_method = case_when(
        str_detect(Spec, "\\[MI\\]") ~ "MI",
        str_detect(Spec, "\\[NO MI\\]") ~ "No MI",
        TRUE ~ "UNKNOWN"
      ),
      
      # Extract weight method - keep all 5 categories separate
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
      )
    )
  
  return(method_mapping)
}

# Function to count coverage issues
count_coverage_issues <- function(batch_output, method_mapping) {
  
  # Join batch output with method mapping (handle many-to-many relationship)
  results <- batch_output %>%
    left_join(method_mapping, by = "suffix", relationship = "many-to-many") %>%
    filter(!is.na(scenario)) %>%
    # Count parameters with coverage outside 0.92-0.97 range
    mutate(
      coverage_issue = ifelse(Coverage < 0.92 | Coverage > 0.97, 1, 0)
    ) %>%
    # Remove duplicates that might arise from many-to-many join
    distinct(suffix, Parm, scenario, proc_type, correlation, missing_method, 
             weight_method, outcome_type, visit_restriction, .keep_all = TRUE) %>%
    group_by(scenario, proc_type, correlation, missing_method, weight_method, 
             outcome_type, visit_restriction) %>%
    summarise(
      total_params = n(),
      coverage_issues = sum(coverage_issue, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(results)
}

# Function to create the summary table
create_coverage_table <- function(coverage_results) {
  
  # Remove the filter for continuous outcomes and GENMOD procedure
  table_data <- coverage_results %>%
    # Create description based on scenario and other characteristics
    mutate(
      description = case_when(
        scenario == 1 & visit_restriction == "full_sample" ~ "1 - MI + V1 Weight (Full Sample)",
        scenario == 2 & visit_restriction == "full_sample" ~ "2 - No MI + V1 Weight (Full Sample)", 
        scenario == 3 & visit_restriction == "V3_restricted" ~ "3 - MI + V3 Adjusted Weight (Restricted Sample)",
        scenario == 4 & visit_restriction == "V3_restricted" ~ "4 - No MI + V3 Adjusted Weight (Restricted Sample)",
        scenario == 5 & visit_restriction == "visit_specific" ~ "5 - MI + Visit-specific Weights (Full Sample)",
        scenario == 6 & visit_restriction == "visit_specific" ~ "6 - No MI + Visit-specific Weights (Full Sample)",
        TRUE ~ paste("Scenario", scenario)
      )
    )
  
  # Pivot to create the desired table format
  table_wide <- table_data %>%
    select(description, outcome_type, proc_type, correlation, weight_method, coverage_issues) %>%
    # Keep all 5 weight method categories separate
    pivot_wider(
      names_from = c(outcome_type, proc_type, correlation, weight_method),
      values_from = coverage_issues,
      names_sep = "_"
    ) %>%
    arrange(description)
  
  return(table_wide)
}

# Function to format the final table for display
format_coverage_table <- function(table_wide) {
  
  # Check what columns actually exist
  cat("Available columns in table_wide:", paste(names(table_wide), collapse = ", "), "\n")
  
  # Create the formatted table based on available columns
  formatted_table <- table_wide
  
  # Since we now have all combinations, create a more comprehensive mapping
  # The columns will be named like: Outcome_Procedure_Correlation_WeightMethod
  # Examples: Continuous_GENMOD_Independent_RR_glm, Binary_GEE_Exchangeable_RR_NRadj_strat, etc.
  
  # Create shorter, more readable column names
  formatted_table <- formatted_table %>%
    rename_with(~ gsub("Continuous_", "Cont_", .x)) %>%
    rename_with(~ gsub("Binary_", "Bin_", .x)) %>%
    rename_with(~ gsub("GENMOD_", "GM_", .x)) %>%
    rename_with(~ gsub("GEE_", "GEE_", .x)) %>%
    rename_with(~ gsub("Independent_", "Ind_", .x)) %>%
    rename_with(~ gsub("Exchangeable_", "Exch_", .x)) %>%
    rename_with(~ gsub("RR_glm_agestrat_strat", "RR_glm_age", .x)) %>%
    rename_with(~ gsub("RR_glm_strat", "RR_glm_str", .x)) %>%
    rename_with(~ gsub("RR_NRadj_strat", "RR_NRadj_str", .x))
  
  # Replace NA with "-" for better display
  formatted_table <- formatted_table %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.x), "-", as.character(.x))))
  
  return(formatted_table)
}

# Main execution
cat("=== Creating method mapping ===\n")
method_mapping <- create_method_mapping(method_codes)

cat("=== Counting coverage issues ===\n") 
coverage_results <- count_coverage_issues(batch_output, method_mapping)

cat("=== Creating comprehensive coverage table ===\n")
# Create one comprehensive table with all data
comprehensive_table <- create_coverage_table(coverage_results)
comprehensive_final <- format_coverage_table(comprehensive_table)

# Display the comprehensive results
cat("\n=== COMPREHENSIVE COVERAGE ISSUES SUMMARY ===\n")
cat("Number of parameters with coverage outside 0.92-0.97 range\n")
cat("Organized by: Outcome Type | Procedure | Correlation | Weight Method\n\n")
print(comprehensive_final, row.names = FALSE)

# Create filtered summaries for easier viewing
cat("\n=== CONTINUOUS OUTCOME, PROC GENMOD ===\n")
continuous_genmod_detailed <- coverage_results %>%
  filter(outcome_type == "Continuous", proc_type == "GENMOD") %>%
  arrange(scenario, correlation, weight_method) %>%
  mutate(description_text = paste("Scenario", scenario)) %>%
  select(scenario, description_text, correlation, weight_method, 
         total_params, coverage_issues)
print(continuous_genmod_detailed, row.names = FALSE)

cat("\n=== BINARY OUTCOME, PROC GENMOD ===\n")
binary_genmod_detailed <- coverage_results %>%
  filter(outcome_type == "Binary", proc_type == "GENMOD") %>%
  arrange(scenario, correlation, weight_method) %>%
  mutate(description_text = paste("Scenario", scenario)) %>%
  select(scenario, description_text, correlation, weight_method, 
         total_params, coverage_issues)
print(binary_genmod_detailed, row.names = FALSE)

# Summary by weight method to show the 5 categories
cat("\n=== Summary by Weight Method (All 5 Categories) ===\n")
weight_summary <- coverage_results %>%
  group_by(weight_method, outcome_type, proc_type) %>%
  summarise(
    scenarios = n_distinct(scenario),
    total_coverage_issues = sum(coverage_issues),
    avg_coverage_issues = round(mean(coverage_issues), 2),
    .groups = "drop"
  ) %>%
  arrange(outcome_type, proc_type, weight_method)
print(weight_summary)

# Save results
coverage_summary <- list(
  method_mapping = method_mapping,
  coverage_results = coverage_results,
  comprehensive_table = comprehensive_final,
  continuous_genmod = continuous_genmod_detailed,
  binary_genmod = binary_genmod_detailed,
  weight_method_summary = weight_summary
)

# Return the summary for further use
coverage_summary