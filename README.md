# morphStuart
Some data cleaning and analysis


# Table of Functions
## Morph
- *ToDo* Extract Fish IDs
- *ToDo* Basic quality checks
- *ToDo* Flag fish without scales
- morph_corrected <- handle_special_cases(morph)
- overlap_results <- identify_overlaps(morph_corrected, threshold = 0.05)
- merged_non_overlap <- merge_non_overlap(overlap_results$non_overlap_fish)
  - *ToDo* export list
- review_results <- create_review_list(overlap_results, use_threshold = TRUE)
  - *ToDo* export list
- filtered_metrics <- review_results$filtered_metrics
- *ToDo* four plotting functions


## PaleoEco
- paleo_metadata <- extract_paleo_metadata(paleo)
- *ToDo* quality checks
- paleo_metadata <- extract_paleo_metadata(paleo)
  - optional single morphotype processing
- results <- create_morphotype_matrix_split(merged_all_counts, paleo_with_ages)
- files <- export_morphotype_results(results, "paleo_morphotypes", p$data.intermediate)
- *combines FieldOrder* paleo_with_ages <- integrate_age_data(paleo_metadata, morph_with_age)
- *ToDo* create rioja matrix
- *ToDo* rioja plotting functions
- *ToDo* export and quality check functions

## FieldOrder
- *ToDo* format and standardize age data
- *ToDo* identify and handle duplicates
- *ToDo* create clean dataset
- *ToDo* identify records with missing data
- *ToDo* flag problematic data
- *ToDo* export data
- *ToDo* plotting functions


## Validation
- 

## Combined
- 
