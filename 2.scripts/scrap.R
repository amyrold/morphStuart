#==============================================================================#
# PALEO ECO DATABASE
Transform tidy paleo data to rioja-compatible format
```{r}
paleo <- read.csv(paste0(p$`1.data/a.raw`, "060625_paleoeco_seriesL.csv"))
str(paleo)
```


```{r}
# ============================================================================ #
# HELPER FUNCTIONS FOR KILLIFISH/STICKLEBACK ANALYSIS
# ============================================================================ #

# Check killifish vs stickleback distribution
check_sample_types <- function(paleo) {
  cat("=== DETAILED KILLIFISH/STICKLEBACK CHECK ===\n")
  
  sample_check <- paleo %>%
    distinct(Sample_ID) %>%
    mutate(
      sample_type = case_when(
        str_detect(Sample_ID, "LXXXX") ~ "Killifish",
        str_detect(Sample_ID, "L\\d+") ~ "Stickleback", 
        TRUE ~ "Unknown"
      ),
      L_pattern = str_extract(Sample_ID, "L[^_]*")
    )
  
  cat("Sample breakdown:\n")
  type_summary <- table(sample_check$sample_type)
  print(type_summary)
  
  cat("\nUnique L-patterns:\n")
  print(table(sample_check$L_pattern))
  
  cat("\nExample Sample IDs:\n")
  examples <- sample_check %>%
    group_by(sample_type) %>%
    slice_head(n = 3)
  print(examples)
  
  return(sample_check)
}

# Compare microfossil assemblages between sample types
compare_assemblages <- function(paleo, microfossil_type = "Diatom") {
  cat("=== COMPARING ASSEMBLAGES BETWEEN SAMPLE TYPES ===\n")
  
  comparison <- paleo %>%
    filter(Microfossil_Type == microfossil_type, Count > 0) %>%
    mutate(
      sample_type = case_when(
        str_detect(Sample_ID, "LXXXX") ~ "Killifish",
        str_detect(Sample_ID, "L\\d+") ~ "Stickleback", 
        TRUE ~ "Unknown"
      ),
      Taxon = case_when(
        !is.na(Genus_Type) & !is.na(Species) & 
          Genus_Type != "" & Species != "" ~ paste(Genus_Type, Species, sep = "_"),
        !is.na(Genus_Type) & Genus_Type != "" ~ paste(Genus_Type, "spp", sep = "_"),
        !is.na(Morphotype) & Morphotype != "" ~ Morphotype,
        TRUE ~ "Unknown"
      )
    ) %>%
    filter(sample_type != "Unknown", Taxon != "Unknown")
  
  # Summary by sample type
  assemblage_summary <- comparison %>%
    group_by(sample_type) %>%
    summarise(
      n_samples = n_distinct(Sample_ID),
      n_taxa = n_distinct(Taxon),
      total_specimens = sum(Count),
      avg_specimens_per_sample = round(total_specimens / n_samples, 1),
      .groups = "drop"
    )
  
  cat("\nAssemblage comparison:\n")
  print(assemblage_summary)
  
  # Top taxa in each sample type
  cat("\nTop taxa by sample type:\n")
  top_taxa <- comparison %>%
    group_by(sample_type, Taxon) %>%
    summarise(total_count = sum(Count), .groups = "drop") %>%
    group_by(sample_type) %>%
    slice_max(total_count, n = 5) %>%
    arrange(sample_type, desc(total_count))
  
  print(top_taxa)
  
  return(list(summary = assemblage_summary, top_taxa = top_taxa))
}

# Connect paleo and morphology data using V-numbers and age  
connect_paleo_morphology <- function(rioja_data, morph_with_age) {
  cat("=== CONNECTING PALEO AND MORPHOLOGY DATA ===\n")
  
  # Extract sample info from paleo data
  paleo_samples <- rioja_data$samples %>%
    filter(!is.na(V_number)) %>%
    select(V_number, bin, sample_type, CSTRAT, YEAR) %>%
    distinct()
  
  # Get unique morph samples with age data
  morph_samples <- morph_with_age %>%
    filter(!is.na(fish_id)) %>%
    select(fish_id, bin, CSTRAT, YEAR) %>%
    distinct() %>%
    # Extract V-number from fish_id  
    mutate(V_number = str_extract(fish_id, "V\\d+"))
  
  # Find direct matches by V-number and bin
  direct_matches <- inner_join(paleo_samples, morph_samples, 
                               by = c("V_number", "bin"), 
                               suffix = c("_paleo", "_morph"))
  
  # Find matches by bin only (same stratigraphic level, different specimens)
  bin_matches <- inner_join(
    paleo_samples %>% select(bin, sample_type, CSTRAT, YEAR) %>% distinct(),
    morph_samples %>% select(bin, CSTRAT, YEAR) %>% distinct(),
    by = "bin",
    suffix = c("_paleo", "_morph")
  )
  
  cat("Paleo samples with V-numbers:", nrow(paleo_samples), "\n")
  cat("Morph samples with V-numbers:", nrow(morph_samples), "\n") 
  cat("Direct sample matches (same V-number + bin):", nrow(direct_matches), "\n")
  cat("Stratigraphic level matches (same bin):", nrow(bin_matches), "\n")
  
  # Check age/depth consistency
  if(nrow(direct_matches) > 0) {
    cat("\nDirect matches - age/depth consistency check:\n")
    age_check <- direct_matches %>%
      filter(!is.na(YEAR_paleo) & !is.na(YEAR_morph)) %>%
      mutate(age_diff = abs(YEAR_paleo - YEAR_morph)) %>%
      summarise(
        n_with_ages = n(),
        mean_age_diff = mean(age_diff),
        max_age_diff = max(age_diff)
      )
    print(age_check)
  }
  
  if(nrow(bin_matches) > 0) {
    cat("\nStratigraphic level matches - age/depth consistency:\n")
    strat_check <- bin_matches %>%
      filter(!is.na(YEAR_paleo) & !is.na(YEAR_morph)) %>%
      mutate(age_diff = abs(YEAR_paleo - YEAR_morph)) %>%
      summarise(
        n_levels = n(),
        mean_age_diff = mean(age_diff),
        max_age_diff = max(age_diff)
      )
    print(strat_check)
  }
  
  return(list(
    direct_matches = direct_matches,
    stratigraphic_matches = bin_matches
  ))
}

# ============================================================================ #
# PRACTICAL PALEO DATA ANALYSIS FOR RIOJA
# This is YOUR working script - use these functions with your actual data
# ============================================================================ #

# FUNCTION 1: Explore your data structure (UPDATED FOR KILLIFISH CHECK)
explore_paleo <- function(paleo) {
  cat("=== YOUR PALEO DATA OVERVIEW ===\n")
  cat("Total rows:", nrow(paleo), "\n")
  cat("Unique samples:", length(unique(paleo$Sample_ID)), "\n")
  cat("Microfossil types:", paste(unique(paleo$Microfossil_Type), collapse = ", "), "\n")
  cat("Date range of samples:", min(paleo$Sample_ID), "to", max(paleo$Sample_ID), "\n")
  
  # Check count distribution
  cat("\nCount summary:\n")
  print(summary(paleo$Count))
  
  # Sample counts by type
  cat("\nCounts by microfossil type:\n")
  type_counts <- paleo %>%
    group_by(Microfossil_Type) %>%
    summarise(
      total_specimens = sum(Count),
      n_samples = n_distinct(Sample_ID),
      .groups = "drop"
    )
  print(type_counts)
  
  # DETAILED CHECK FOR KILLIFISH VS STICKLEBACK SAMPLES
  cat("\n=== SAMPLE ID ANALYSIS (CHECKING FOR KILLIFISH) ===\n")
  
  sample_analysis <- paleo %>%
    distinct(Sample_ID) %>%
    mutate(
      # Check for different L-patterns
      has_L_digits = str_detect(Sample_ID, "L\\d+"),  # L followed by digits
      has_L_X = str_detect(Sample_ID, "LXXXX"),       # Literal LXXXX
      has_L_other = str_detect(Sample_ID, "L[^\\d]"), # L followed by non-digits
      
      # Extract various patterns
      L_pattern = str_extract(Sample_ID, "L[^_]*"),   # Everything after L until _
      strat_number = as.numeric(str_extract(Sample_ID, "(?<=L)\\d+")),
      
      # Classify sample type
      sample_type = case_when(
        str_detect(Sample_ID, "LXXXX") ~ "Killifish",
        has_L_digits ~ "Stickleback", 
        TRUE ~ "Unknown"
      )
    )
  
  cat("Sample type breakdown:\n")
  print(table(sample_analysis$sample_type))
  
  cat("\nUnique L-patterns found:\n")
  print(table(sample_analysis$L_pattern, useNA = "ifany"))
  
  # Show examples of each type
  cat("\nExample Sample IDs by type:\n")
  examples <- sample_analysis %>%
    group_by(sample_type) %>%
    slice_head(n = 3) %>%
    select(Sample_ID, sample_type, L_pattern, strat_number)
  print(examples)
  
  # Check for samples that might not be parsed correctly
  problematic_samples <- sample_analysis %>%
    filter(is.na(strat_number) | sample_type == "Unknown")
  
  if(nrow(problematic_samples) > 0) {
    cat("\nSamples that might need special handling:\n")
    print(problematic_samples)
  }
  
  # Return the analysis for further use
  return(sample_analysis)
}

# FUNCTION 2: Convert your data to rioja format (UPDATED FOR JACOPO'S REQUIREMENTS)
convert_to_rioja <- function(paleo, microfossil_type = "Diatom", 
                             min_total_count = 0, min_samples = 1,  # Changed defaults to include all taxa initially
                             include_killifish = TRUE) {
  
  cat("=== CONVERTING TO RIOJA FORMAT (JACOPO'S SPECIFICATIONS) ===\n")
  cat("Focusing on:", microfossil_type, "\n")
  cat("Including killifish samples:", include_killifish, "\n")
  cat("Minimum total count per taxon:", min_total_count, "\n")
  cat("Minimum samples per taxon:", min_samples, "\n")
  
  # Step 1: CRITICAL - Merge multiple counts per sample first
  cat("Merging multiple microscopy line counts per sample...\n")
  paleo_merged <- paleo %>%
    filter(Microfossil_Type == microfossil_type) %>%
    # Group by sample and taxonomic identity, then sum counts
    group_by(Sample_ID, Microfossil_Type, Morphotype, Genus_Type, Species, Variety) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")
  
  # Step 2: Create taxonomic names according to Jacopo's preferences
  paleo_clean <- paleo_merged %>%
    mutate(
      # For diatoms: Use morphotype level as Jacopo requested
      # For phytoliths: Would need to create grassy vs woody groups
      Taxon = case_when(
        microfossil_type == "Diatom" & !is.na(Morphotype) & Morphotype != "" ~ Morphotype,
        microfossil_type == "Diatom" & !is.na(Genus_Type) & Genus_Type != "" ~ Genus_Type,
        microfossil_type == "Phytolith" & !is.na(Morphotype) & Morphotype != "" ~ Morphotype,
        !is.na(Genus_Type) & Genus_Type != "" ~ Genus_Type,
        TRUE ~ "Unknown"
      ),
      
      # Handle both stickleback and killifish samples
      sample_type = case_when(
        str_detect(Sample_ID, "LXXXX") ~ "Killifish",
        str_detect(Sample_ID, "L\\d+") ~ "Stickleback", 
        TRUE ~ "Unknown"
      ),
      
      # Extract V-number for linking with morph data
      V_number = str_extract(Sample_ID, "V\\d+"),
      
      # Extract L-number (field excavation number)
      L_number = str_extract(Sample_ID, "L\\w+"),
      
      # For now, use L-number for ordering (will need Mike Bell's ages later)
      strat_level = case_when(
        sample_type == "Stickleback" ~ as.numeric(str_extract(Sample_ID, "(?<=L)\\d+")),
        sample_type == "Killifish" ~ 9999,  # Place at top for now
        TRUE ~ NA_real_
      ),
      
      # Create informative sample labels
      sample_label = case_when(
        sample_type == "Killifish" ~ paste0("Killifish_", L_number),
        sample_type == "Stickleback" ~ L_number,
        TRUE ~ Sample_ID
      )
    ) %>%
    # Remove zero counts and unknown taxa
    filter(Count > 0, !is.na(strat_level), Taxon != "Unknown")
  
  # Filter samples based on user preference
  if (!include_killifish) {
    paleo_clean <- paleo_clean %>%
      filter(sample_type == "Stickleback")
    cat("Excluding killifish samples\n")
  }
  
  # Step 3: Apply minimal filtering (include almost everything as Jacopo requested)
  common_taxa <- paleo_clean %>%
    group_by(Taxon) %>%
    summarise(
      total_count = sum(Count),
      n_samples = n_distinct(Sample_ID),
      .groups = "drop"
    ) %>%
    filter(total_count >= min_total_count, n_samples >= min_samples)
  
  cat("\nTaxonomic summary:\n")
  cat("Started with", length(unique(paleo_clean$Taxon)), "taxa\n")
  cat("Kept", nrow(common_taxa), "taxa after filtering\n")
  
  # Show taxa list for Jacopo to review
  cat("\nTaxa included (for Jacopo's ecological grouping):\n")
  taxa_for_review <- common_taxa %>%
    arrange(desc(total_count)) %>%
    mutate(rank = row_number())
  print(taxa_for_review)
  
  # Step 4: Convert to wide format
  paleo_wide <- paleo_clean %>%
    filter(Taxon %in% common_taxa$Taxon) %>%
    # Sum any remaining duplicates within samples
    group_by(Sample_ID, Taxon, strat_level, sample_type, sample_label, V_number, L_number) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    # Pivot to wide format
    pivot_wider(
      names_from = Taxon,
      values_from = Count,
      values_fill = 0
    ) %>%
    # Sort by stratigraphic level
    arrange(desc(strat_level))
  
  # Step 5: Separate sample info from count data
  sample_info <- paleo_wide %>%
    select(Sample_ID, strat_level, sample_type, sample_label, V_number, L_number)
  
  count_matrix <- paleo_wide %>%
    select(-Sample_ID, -strat_level, -sample_type, -sample_label, -V_number, -L_number) %>%
    as.data.frame()
  
  # Set row names for rioja
  rownames(count_matrix) <- sample_info$sample_label
  
  cat("\nFinal dataset:", nrow(count_matrix), "samples ×", ncol(count_matrix), "taxa\n")
  
  # Return everything including taxa list for Jacopo
  return(list(
    counts = count_matrix,
    samples = sample_info,
    taxa_for_review = taxa_for_review,  # This is what Jacopo needs to see
    microfossil_type = microfossil_type
  ))
}

# FUNCTION 3: Create stratigraphic plot (rioja version)
create_strat_plot <- function(rioja_data, use_percentages = TRUE, 
                              plot_title = NULL) {
  
  cat("=== CREATING STRATIGRAPHIC PLOT (RIOJA) ===\n")
  
  # Prepare data
  plot_data <- rioja_data$counts
  depths <- rioja_data$samples$strat_level
  
  if (use_percentages) {
    plot_data <- plot_data / rowSums(plot_data) * 100
    cat("Using percentages\n")
  } else {
    cat("Using raw counts\n")
  }
  
  # Set up plot title
  if (is.null(plot_title)) {
    plot_title <- paste(rioja_data$microfossil_type, "Stratigraphy")
  }
  
  # Create the plot with absolute minimal parameters
  par(mar = c(5, 4, 4, 8))
  
  # Try the most basic strat.plot call possible
  try({
    strat.plot(plot_data, yvar = depths)
    title(main = plot_title)
  }, silent = FALSE)
  
  cat("Plot created successfully!\n")
  return(plot_data)
}

# FUNCTION 3B: Create proper stratigraphic plot (UPDATED FOR REAL DEPTHS/AGES)
create_proper_strat_plot <- function(rioja_data, use_percentages = TRUE, 
                                     plot_title = NULL, depth_axis = "CSTRAT") {
  
  cat("=== CREATING PROPER STRATIGRAPHIC PLOT (WITH REAL DEPTHS/AGES) ===\n")
  
  plot_data <- rioja_data$counts
  samples <- rioja_data$samples
  
  if (use_percentages) {
    plot_data <- plot_data / rowSums(plot_data) * 100
    cat("Using percentages\n")
  } else {
    cat("Using raw counts\n")
  }
  
  # Set up plot title
  if (is.null(plot_title)) {
    plot_title <- paste(rioja_data$microfossil_type, "Stratigraphy")
  }
  
  # Determine what to use for Y-axis based on available data
  y_axis_data <- samples$strat_level  # Default fallback
  y_label <- "Stratigraphic Level"
  
  if (rioja_data$has_age_data && depth_axis %in% names(samples)) {
    if (depth_axis == "CSTRAT" && "CSTRAT" %in% names(samples)) {
      y_axis_data <- samples$CSTRAT
      y_label <- "Stratigraphic Depth (CSTRAT, cm)"
      cat("Using CSTRAT for Y-axis\n")
    } else if (depth_axis == "age_years" && "age_years" %in% names(samples)) {
      y_axis_data <- samples$age_years
      y_label <- "Age (years)"
      cat("Using age for Y-axis\n")
    } else if (depth_axis == "YEAR" && "YEAR" %in% names(samples)) {
      y_axis_data <- samples$YEAR
      y_label <- "Age (years)"
      cat("Using YEAR for Y-axis\n")
    }
  } else {
    cat("Using L-number based positioning for Y-axis\n")
  }
  
  # Convert to long format
  plot_long <- plot_data %>%
    bind_cols(samples) %>%
    mutate(y_position = y_axis_data) %>%
    pivot_longer(cols = all_of(names(plot_data)), 
                 names_to = "Taxon", values_to = "Abundance")
  
  # Create taxon positions for side-by-side layout
  taxa_order <- names(plot_data)
  plot_long$taxon_position <- match(plot_long$Taxon, taxa_order)
  
  # Calculate scaled abundance for plotting (each taxon gets equal width)
  plot_long <- plot_long %>%
    group_by(Taxon) %>%
    mutate(
      max_abundance = max(Abundance, na.rm = TRUE),
      scaled_abundance = if_else(max_abundance > 0, Abundance / max_abundance * 0.8, 0),
      x_start = taxon_position - 0.4,
      x_end = taxon_position - 0.4 + scaled_abundance
    ) %>%
    ungroup()
  
  # Create colors/fills based on sample type
  plot_long <- plot_long %>%
    mutate(
      fill_color = case_when(
        sample_type == "Killifish" ~ "lightcoral",
        sample_type == "Stickleback" ~ "lightblue",
        TRUE ~ "lightgray"
      ),
      outline_color = case_when(
        sample_type == "Killifish" ~ "darkred",
        sample_type == "Stickleback" ~ "darkblue", 
        TRUE ~ "black"
      )
    )
  
  # Create the stratigraphic plot
  p <- ggplot(plot_long) +
    # Add the abundance curves with different colors for different sample types
    geom_ribbon(aes(xmin = x_start, xmax = x_end, y = y_position, 
                    fill = sample_type, color = sample_type, group = interaction(Taxon, sample_type)),
                alpha = 0.7, size = 0.3) +
    # Add vertical lines at each taxon position
    geom_vline(xintercept = 1:length(taxa_order) - 0.4, 
               color = "gray50", linetype = "dashed", alpha = 0.5) +
    # Add horizontal line to separate killifish from stickleback if both present
    {if("Killifish" %in% plot_long$sample_type && "Stickleback" %in% plot_long$sample_type) {
      geom_hline(yintercept = min(plot_long$y_position[plot_long$sample_type == "Killifish"], na.rm = TRUE) - 
                   (max(plot_long$y_position, na.rm = TRUE) - min(plot_long$y_position, na.rm = TRUE)) * 0.05,
                 color = "red", linetype = "solid", size = 1)
    }} +
    # Custom color scale
    scale_fill_manual(values = c("Killifish" = "lightcoral", "Stickleback" = "lightblue"),
                      name = "Sample Type") +
    scale_color_manual(values = c("Killifish" = "darkred", "Stickleback" = "darkblue"),
                       name = "Sample Type") +
    # Customize axes
    scale_x_continuous(
      breaks = 1:length(taxa_order),
      labels = taxa_order,
      expand = c(0.02, 0)
    ) +
    scale_y_continuous(expand = c(0.01, 0)) +
    # Labels and theme
    labs(
      title = plot_title,
      x = "Taxa",
      y = y_label,
      caption = paste0(
        if(use_percentages) "Abundance shown as % of total" else "Abundance (counts)",
        if("Killifish" %in% plot_long$sample_type) "\nRed = Killifish samples (post-stickleback)" else "",
        if(rioja_data$has_age_data) "\nUsing Mike Bell's age/depth estimates" else ""
      )
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )
  
  # Add annotation if killifish are present
  if("Killifish" %in% plot_long$sample_type) {
    p <- p + annotate("text", x = length(taxa_order)/2, 
                      y = min(plot_long$y_position[plot_long$sample_type == "Killifish"], na.rm = TRUE),
                      label = "↑ Post-stickleback (Killifish)", 
                      color = "darkred", size = 3, hjust = 0.5)
  }
  
  print(p)
  cat("Proper stratigraphic plot created with real depths/ages!\n")
  return(p)
}

# FUNCTION 4: Calculate diversity metrics
calculate_diversity <- function(rioja_data) {
  
  cat("=== CALCULATING DIVERSITY METRICS ===\n")
  
  # Make sure vegan is loaded
  if (!require("vegan", quietly = TRUE)) {
    stop("Please install the vegan package: install.packages('vegan')")
  }
  
  counts <- rioja_data$counts
  samples <- rioja_data$samples
  
  diversity_data <- data.frame(
    Sample_ID = samples$Sample_ID,
    strat_level = samples$strat_level,
    richness = apply(counts > 0, 1, sum),        # Number of taxa
    total_count = rowSums(counts),               # Total specimens
    shannon = vegan::diversity(counts, index = "shannon"),  # Shannon diversity (explicit package call)
    simpson = vegan::diversity(counts, index = "simpson")   # Simpson diversity (explicit package call)
  ) %>%
    arrange(desc(strat_level))  # Oldest first
  
  # Add sample type if available
  if ("sample_type" %in% names(samples)) {
    diversity_data$sample_type <- samples$sample_type
  }
  
  cat("Diversity calculated for", nrow(diversity_data), "samples\n")
  return(diversity_data)
}

# FUNCTION 5: Plot diversity trends
plot_diversity <- function(diversity_data, metric = "shannon") {
  
  cat("=== PLOTTING DIVERSITY TRENDS ===\n")
  
  if (metric == "shannon") {
    p <- ggplot(diversity_data, aes(x = shannon, y = strat_level)) +
      labs(x = "Shannon Diversity", title = "Shannon Diversity Through Section")
  } else if (metric == "richness") {
    p <- ggplot(diversity_data, aes(x = richness, y = strat_level)) +
      labs(x = "Species Richness", title = "Species Richness Through Section")
  } else if (metric == "total_count") {
    p <- ggplot(diversity_data, aes(x = total_count, y = strat_level)) +
      labs(x = "Total Count", title = "Total Specimen Count Through Section")
  }
  
  # Add color coding if sample_type is available
  if ("sample_type" %in% names(diversity_data)) {
    p <- p +
      geom_line(aes(color = sample_type)) +
      geom_point(aes(color = sample_type), size = 2) +
      scale_color_manual(values = c("Killifish" = "darkred", "Stickleback" = "darkblue"),
                         name = "Sample Type")
  } else {
    p <- p +
      geom_line(color = "blue") +
      geom_point(size = 2, color = "darkblue")
  }
  
  p <- p +
    labs(y = "Stratigraphic Level") +
    theme_minimal() +
    theme(axis.text = element_text(size = 10))
  
  print(p)
  return(p)
}

# FUNCTION 6: Export data in Jacopo's requested format
export_for_jacopo <- function(rioja_data, filename_prefix = "paleoeco_summary") {
  
  cat("=== EXPORTING DATA FOR JACOPO ===\n")
  
  # 1. Summary table (taxa as columns, samples as rows) - this is the main request
  summary_table <- rioja_data$counts
  summary_table$Sample_ID <- rioja_data$samples$Sample_ID
  summary_table$V_number <- rioja_data$samples$V_number
  summary_table$L_number <- rioja_data$samples$L_number
  summary_table$sample_type <- rioja_data$samples$sample_type
  
  # Reorder columns to put sample info first
  summary_table <- summary_table %>%
    select(Sample_ID, V_number, L_number, sample_type, everything())
  
  write.csv(summary_table, paste0(filename_prefix, "_counts_by_sample.csv"), row.names = FALSE)
  
  # 2. Taxa list for Jacopo to group ecologically
  write.csv(rioja_data$taxa_for_review, paste0(filename_prefix, "_taxa_for_grouping.csv"), row.names = FALSE)
  
  # 3. Sample information with linking data
  sample_linking <- rioja_data$samples %>%
    select(Sample_ID, V_number, L_number, sample_type, strat_level) %>%
    arrange(V_number)
  
  write.csv(sample_linking, paste0(filename_prefix, "_sample_linking_info.csv"), row.names = FALSE)
  
  cat("Exported files:\n")
  cat("1.", paste0(filename_prefix, "_counts_by_sample.csv"), "(main summary table)\n")
  cat("2.", paste0(filename_prefix, "_taxa_for_grouping.csv"), "(for ecological grouping)\n") 
  cat("3.", paste0(filename_prefix, "_sample_linking_info.csv"), "(for linking with morph data)\n")
  
  return(list(
    summary_table = summary_table,
    taxa_list = rioja_data$taxa_for_review,
    linking_info = sample_linking
  ))
}
plot_diversity <- function(diversity_data, metric = "shannon") {
  
  cat("=== PLOTTING DIVERSITY TRENDS ===\n")
  
  if (metric == "shannon") {
    p <- ggplot(diversity_data, aes(x = shannon, y = strat_level)) +
      labs(x = "Shannon Diversity", title = "Shannon Diversity Through Section")
  } else if (metric == "richness") {
    p <- ggplot(diversity_data, aes(x = richness, y = strat_level)) +
      labs(x = "Species Richness", title = "Species Richness Through Section")
  } else if (metric == "total_count") {
    p <- ggplot(diversity_data, aes(x = total_count, y = strat_level)) +
      labs(x = "Total Count", title = "Total Specimen Count Through Section")
  }
  
  p <- p +
    geom_line(color = "blue") +
    geom_point(size = 2, color = "darkblue") +
    labs(y = "Stratigraphic Level") +
    theme_minimal() +
    theme(axis.text = element_text(size = 10))
  
  print(p)
  return(p)
}



#==============================================================================#
#SCRAP
## Creating morph_with_pgna

morph <- read.csv(paste0(p$`1.data/a.raw`, "241015_PitLMorph.csv"))
pgna <- read.csv(paste0(p$`1.data/a.raw`, "PGNA_PitLMorph.csv"))

# First, let's understand the tpgna duplicates
pgna_summary <- tpgna %>%
  group_by(ID) %>%
  summarise(
    count = n(),
    unique_values = n_distinct(PGNA),
    min_pgna = min(PGNA),
    max_pgna = max(PGNA),
    .groups = "drop"
  )

# Check if there are conflicting values within the same ID
conflicting_pgna <- pgna_summary %>%
  filter(unique_values > 1)

# Aggregate tpgna (taking max value for binary flags, similar to your other binary vars)
pgna_agg <- pgna %>%
  group_by(ID) %>%
  summarise(
    PGNA = max(PGNA, na.rm = TRUE),
    .groups = "drop"
  )

# Now join with the aggregated data
morph_with_pgna <- morph %>%
  left_join(pgna_agg, by = "ID")

write.csv(morph_with_pgna, file = paste0(p$`1.data/a.raw`, "020625_PitLMorph.csv"), row.names = FALSE)



# STEP 2: Explore the data
strat_info <- explore_paleo(paleo)

# STEP 3: Convert to rioja format (start with diatoms)
rioja_data <- convert_to_rioja(paleo, 
                               morph_with_age = morph_with_age,  # Your existing dataset
                               microfossil_type = "Diatom",
                               use_ages = TRUE)

# STEP 4: Create stratigraphic plot
strat_plot_data <- create_strat_plot(rioja_data)

# OR if that fails, use the ggplot version
proper_strat_plot <- create_proper_strat_plot(rioja_data, use_percentages = TRUE)

# STEP 5: Calculate and plot diversity
diversity_metrics <- calculate_diversity(rioja_data)
plot_diversity(diversity_metrics, metric = "shannon")
plot_diversity(diversity_metrics, metric = "richness")

# STEP 6: Save your results
write.csv(rioja_data$counts, "diatom_counts_wide.csv", row.names = TRUE)
write.csv(strat_plot_data, "diatom_percentages.csv", row.names = TRUE)
write.csv(diversity_metrics, "diversity_metrics.csv", row.names = FALSE)

```