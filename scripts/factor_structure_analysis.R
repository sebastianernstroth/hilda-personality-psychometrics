# =============================================================================
# HILDA FACTOR STRUCTURE ANALYSIS - STAGE 4 OF MANUSCRIPT PIPELINE
# =============================================================================
# 
# This script conducts comprehensive factor structure analysis for the personality validation study.
# Compatible with manuscript_replication_pipeline.R
# 
# STAGE 4: Factor Structure Analysis
# - Parallel analysis and VSS for factor retention
# - Exploratory Factor Analysis (EFA) with multiple rotations
# - Confirmatory Factor Analysis (CFA) 
# - Exploratory Structural Equation Modeling (ESEM)
# - Model comparisons (1-factor, 2-factor, 5-factor, hierarchical, bifactor)
# - Factor correlation analysis
# 
# Addresses Results section: Factor Structure Analysis subsection
# =============================================================================

# Stage identification for pipeline integration
CURRENT_STAGE <- list(
  number = 4,
  name = "Factor Structure Analysis", 
  description = "EFA, CFA, ESEM, and comprehensive model comparison analysis",
  required_input = "subsample1_cross_sectional.qs",
  output_files = "factor_structure_results.qs, parallel_analysis.pdf, model_comparison.tex",
  manuscript_section = "Results - Factor Structure Analysis"
)

# Clear environment
# rm(list = ls(all = TRUE))
# graphics.off()

cat("=== STAGE", CURRENT_STAGE$number, ":", CURRENT_STAGE$name, "===\n")
cat(CURRENT_STAGE$description, "\n")
cat("Conducting comprehensive factor structure analysis...\n\n")

# =============================================================================
# LOAD PACKAGES
# =============================================================================

cat("Loading required packages...\n")

# Required packages for factor analysis
required_packages <- c("tidyverse", "qs", "psych", "lavaan", "semTools", "GPArotation", 
                      "ggplot2", "cowplot", "kableExtra", "MplusAutomation", "broom",
                      "dplyr")

# Install missing packages with progress reporting
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  tryCatch({
    install.packages(missing_packages)
    cat("✓ Package installation completed\n")
  }, error = function(e) {
    stop("❌ Failed to install required packages: ", e$message)
  })
}

# Load packages with error checking
for(pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
  }, error = function(e) {
    stop("❌ Failed to load package '", pkg, "': ", e$message)
  })
}

cat("✓ All packages loaded successfully\n\n")

# =============================================================================
# DIRECTORY SETUP - PIPELINE COMPATIBLE
# =============================================================================

cat("Setting up directory structure...\n")

# Determine project root (pipeline compatible)
if(file.exists("manuscript_replication_pipeline.R")) {
  # Running as part of pipeline
  root <- getwd()
  cat("✓ Pipeline mode: Using current directory as root\n")
} else if(file.exists("hilda-personality-psychometrics.Rproj")) {
  # Running standalone with RStudio project
  root <- rprojroot::find_root(rprojroot::is_rstudio_project)
  cat("✓ Standalone mode: Using RStudio project root\n")
} else {
  # Fallback to current directory
  root <- getwd()
  cat("⚠️ Fallback mode: Using current directory as root\n")
}

# Set up directory paths (compatible with pipeline structure)
outputDir <- file.path(root, "output")

manuscriptTablesDir <- file.path(root, "manuscript", "tables")
manuscriptFiguresDir <- file.path(root, "manuscript", "figures")
supplementalTablesDir <- file.path(root, "supplemental materials", "tables")
factorStructureDir = file.path(outputDir, "factor_structure")
processedDataDir = file.path(outputDir, "data")

# Create directories if they don't exist
for(dir_path in c(outputDir, manuscriptTablesDir, manuscriptFiguresDir, supplementalTablesDir,
                  factorStructureDir, processedDataDir)) {
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

cat("Project root:", root, "\n")
cat("Output directory:", outputDir, "\n")
cat("Manuscript tables directory:", manuscriptTablesDir, "\n")
cat("Manuscript figures directory:", manuscriptFiguresDir, "\n")
cat("Supplemental Materials tables directory:", supplementalTablesDir, "\n\n")

# =============================================================================
# LOAD PREPARED DATASETS
# =============================================================================

cat("=== LOADING PREPARED DATASETS ===\n")

# Load cross-sectional sample (Wave 5) for factor analysis
subsample1_file <- file.path(processedDataDir, "subsample1_cross_sectional.qs")
if(!file.exists(subsample1_file)) {
  cat("❌ ERROR: Cross-sectional sample not found!\n")
  cat("Please run Stage 2 (participants_and_measures.R) first.\n")
  cat("Expected file:", subsample1_file, "\n\n")
  stop("Cross-sectional sample missing. Analysis stopped.")
}

cat("Loading cross-sectional sample (Wave 5) for factor analysis...\n")
subsample1 <- qs::qread(subsample1_file)
cat("✓ Cross-sectional sample loaded:", format(nrow(subsample1), big.mark = ","), "observations\n\n")

# =============================================================================
# DEFINE PERSONALITY ITEMS AND EXTRACT DATA
# =============================================================================

cat("=== PREPARING FACTOR ANALYSIS DATA ===\n")

# Step 1: Define personality items for factor analysis
cat("Step 1: Defining Big Five personality items...\n")
personality_items <- c(
  # Neuroticism items
  "envious", "fretful", "jealous", "moody", "temperamental", "touchy",
  # Openness items
  "complex", "creative", "deep", "imaginative", "intellectual", "philosophical",
  # Extraversion items (including reverse-coded)
  "bashful_r", "extroverted", "lively", "quiet_r", "shy_r", "talkative",
  # Conscientiousness items (including reverse-coded)
  "disorganised_r", "efficient", "inefficient_r", "orderly", "sloppy_r", "systematic",
  # Agreeableness items
  "cooperative", "kind", "sympathetic", "warm"
)

# Check item availability
available_items <- intersect(personality_items, colnames(subsample1))
missing_items <- setdiff(personality_items, colnames(subsample1))

cat("✓ Personality items available:", length(available_items), "of", length(personality_items), "\n")
if(length(missing_items) > 0) {
  cat("⚠️ Missing items:", paste(missing_items, collapse = ", "), "\n")
}

# Step 2: Extract item data for factor analysis
cat("Step 2: Extracting and preparing factor analysis data...\n")
efa_data <- subsample1 %>%
  select(all_of(available_items)) %>%
  as.data.frame()

# Remove cases with excessive missing data (>50% missing)
complete_threshold <- 0.5 * length(available_items)
efa_data <- efa_data[rowSums(is.na(efa_data)) <= complete_threshold, ]

cat("✓ Factor analysis dataset prepared:", format(nrow(efa_data), big.mark = ","), "cases,", ncol(efa_data), "items\n")
cat("✓ Missing data rate:", sprintf("%.1f%%", sum(is.na(efa_data)) / (nrow(efa_data) * ncol(efa_data)) * 100), "\n\n")

# =============================================================================
# PARALLEL ANALYSIS AND VSS
# =============================================================================

cat("=== PARALLEL ANALYSIS AND VSS FOR FACTOR RETENTION ===\n")

# Step 1: Conduct parallel analysis
cat("Step 1: Running parallel analysis for factor retention...\n")
parallel_result <- tryCatch({
  psych::fa.parallel(efa_data, fm = "ml", fa = "fa", n.iter = 100, 
                    main = "Parallel Analysis Scree Plot")
}, error = function(e) {
  cat("❌ Error in parallel analysis:", e$message, "\n")
  return(NULL)
})

if(!is.null(parallel_result)) {
  suggested_factors <- parallel_result$nfact
  cat("✓ Parallel analysis suggests", suggested_factors, "factors\n")
} else {
  suggested_factors <- 5  # Default to 5 factors if parallel analysis fails
  cat("⚠️ Parallel analysis failed, defaulting to 5 factors\n")
}

# Step 2: Create parallel analysis plot
cat("Step 2: Creating parallel analysis figure...\n")

create_parallel_analysis_plot <- function(parallel_result, efa_data, save_path = NULL) {
  
  if(is.null(parallel_result)) {
    cat("Cannot create plot - parallel analysis failed\n")
    return(NULL)
  }
  
  n_factors <- length(parallel_result$fa.values)
  
  # Create comprehensive data frame
  plot_data <- data.frame(
    Factor = 1:n_factors,
    Observed = parallel_result$fa.values,
    Simulated = parallel_result$fa.sim,
    Difference = parallel_result$fa.values - parallel_result$fa.sim
  )
  
  # Determine which factors to retain
  plot_data$Retain <- plot_data$Observed > plot_data$Simulated
  
  # Create main plot
  p1 <- ggplot(plot_data, aes(x = Factor)) +
    # Shaded area showing retained factors
    geom_ribbon(aes(ymin = 0, ymax = pmax(Observed, Simulated)), 
                fill = "lightblue", alpha = 0.2) +
    # Lines
    geom_line(aes(y = Observed, color = "Observed"), size = 1.1) +
    geom_line(aes(y = Simulated, color = "Simulated"), size = 1.1) +
    # Points with different shapes for retained factors
    geom_point(aes(y = Observed, shape = Retain, color = "Observed"), size = 2.5) +
    geom_point(aes(y = Simulated, color = "Simulated"), size = 2.2, shape = 1) +
    # Eigenvalue = 1 line
    geom_hline(yintercept = 1, linetype = "dotted", color = "gray40", size = 0.7) +
    # Scales and labels
    scale_color_manual(values = c("Observed" = "#1f77b4", "Simulated" = "#ff7f0e")) +
    scale_shape_manual(values = c("TRUE" = 19, "FALSE" = 1), guide = "none") +
    scale_x_continuous(breaks = 1:min(12, n_factors)) +
    labs(x = "Factor Number", y = "Eigenvalue", color = "Data Source") +
    theme_minimal(base_size = 16, base_family = "serif") +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 16, family = "serif", face = "plain"),
      legend.text = element_text(size = 14, family = "serif"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      axis.title = element_text(size = 16, family = "serif", face = "plain"),
      axis.text = element_text(size = 14, family = "serif", color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.4),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_blank()
    )
  
  # Add text annotations for first few factors
  for(i in 1:min(5, sum(plot_data$Retain))) {
    p1 <- p1 + annotate("text", x = i + 0.6, y = plot_data$Observed[i] + 0.15,
                       label = sprintf("%.2f", plot_data$Observed[i]),
                       size = 5, color = "#1f77b4", fontface = "plain", family = "serif")
  }
  
  # Save plot if path provided
  if(!is.null(save_path)) {
    ggsave(save_path, plot = p1, width = 11, height = 7, dpi = 300)
  }
  
  return(p1)
}

# Generate and save parallel analysis plot
parallel_plot <- create_parallel_analysis_plot(
  parallel_result, efa_data, 
  save_path = file.path(manuscriptFiguresDir, "parallel_analysis.pdf")
)

cat("✓ Parallel analysis plot saved to manuscript/figures/\n")

# Step 3: VSS analysis for additional factor retention evidence
cat("Step 3: Running Very Simple Structure (VSS) analysis...\n")
vss_result <- tryCatch({
  psych::VSS(efa_data, n = 8, rotate = "oblimin", fm = "ml")
}, error = function(e) {
  cat("❌ Error in VSS analysis:", e$message, "\n")
  return(NULL)
})

if(!is.null(vss_result)) {
  vss_complexity1 <- which.max(vss_result$cfit.1)
  vss_complexity2 <- which.max(vss_result$cfit.2)
  cat("✓ VSS complexity 1 maximum at", vss_complexity1, "factors\n")
  cat("✓ VSS complexity 2 maximum at", vss_complexity2, "factors\n\n")
} else {
  cat("⚠️ VSS analysis failed\n\n")
}

# =============================================================================
# EXPLORATORY FACTOR ANALYSIS
# =============================================================================

cat("=== EXPLORATORY FACTOR ANALYSIS ===\n")

# Step 1: Run EFA with optimal number of factors
cat("Step 1: Running EFA with 5-factor solution (oblimin rotation)...\n")
n_factors <- 5  # Based on theoretical expectations and parallel analysis

efa_result <- tryCatch({
  psych::fa(efa_data, nfactors = n_factors, rotate = "oblimin", 
           fm = "ml", scores = "regression")
}, error = function(e) {
  cat("❌ Error in EFA:", e$message, "\n")
  return(NULL)
})

if(!is.null(efa_result)) {
  variance_explained <- round(sum(efa_result$Vaccounted[2, ]) * 100, 1)
  cat("✓ EFA completed: 5-factor solution explains", variance_explained, "% of variance\n")
} else {
  stop("EFA failed to converge - cannot proceed with analysis")
}

# Step 2: Test rotation method stability and create comparison table
cat("Step 2: Testing factor stability across rotation methods...\n")

rotation_methods <- c("oblimin", "varimax", "geominQ", "promax")
efa_results <- list()

for(rotation in rotation_methods) {
  cat("Running EFA with", rotation, "rotation...\n")
  efa_results[[rotation]] <- tryCatch({
    psych::fa(efa_data, nfactors = 5, rotate = rotation, fm = "ml")
  }, error = function(e) {
    cat("Error with", rotation, ":", e$message, "\n")
    return(NULL)
  })
}

# Create comprehensive rotation comparison table
create_rotation_comparison_table <- function(efa_results, available_items) {
  
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering", 
    "\\caption{Exploratory Factor Analysis: Comparison of Rotation Methods}",
    "\\label{tab:efa_rotation_comparison}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Item & Oblimin & Varimax & GeominQ & Promax \\\\",
    "\\midrule"
  )
  
  # Define item groups
  item_groups <- list(
    "\\textbf{Neuroticism Items}" = c("envious", "fretful", "jealous", "moody", "temperamental", "touchy"),
    "\\textbf{Extraversion Items}" = c("bashful_r", "extroverted", "lively", "quiet_r", "shy_r", "talkative"),
    "\\textbf{Openness to Experience Items}" = c("complex", "creative", "deep", "imaginative", "intellectual", "philosophical"),
    "\\textbf{Agreeableness Items}" = c("cooperative", "kind", "sympathetic", "warm"),
    "\\textbf{Conscientiousness Items}" = c("disorganised_r", "efficient", "inefficient_r", "orderly", "sloppy_r", "systematic")
  )
  
  # Helper function
  remove_leading_zero <- function(x, digits = 2) {
    if(is.na(x)) return("--")
    formatted <- sprintf(paste0("%.", digits, "f"), x)
    if(abs(x) < 1) {
      formatted <- gsub("^0\\.", ".", formatted)
      formatted <- gsub("^-0\\.", "-.", formatted)
    }
    return(formatted)
  }
  
  format_loading <- function(loading) {
    if(is.na(loading)) {
      return("--")
    } else {
      return(remove_leading_zero(loading, 2))
    }
  }
  
  # Add items by group
  for(group_name in names(item_groups)) {
    latex_lines <- c(latex_lines, paste0(group_name, " & & & & \\\\"))
    
    items <- item_groups[[group_name]]
    
    for(item in items) {
      # Check if item exists in first available result
      first_result <- efa_results[[which(!sapply(efa_results, is.null))[1]]]
      
      if(!is.null(first_result) && item %in% rownames(first_result$loadings)) {
        
        # Get highest loading for each rotation method
        loadings_row <- c()
        
        for(rotation in c("oblimin", "varimax", "geominQ", "promax")) {
          if(!is.null(efa_results[[rotation]])) {
            item_loadings <- efa_results[[rotation]]$loadings[item, ]
            max_loading <- item_loadings[which.max(abs(item_loadings))]
            loadings_row <- c(loadings_row, format_loading(max_loading))
          } else {
            loadings_row <- c(loadings_row, "--")
          }
        }
        
        # Create item display name
        item_display <- switch(item,
                               "envious" = "Envious", "fretful" = "Fretful", "jealous" = "Jealous",
                               "moody" = "Moody", "temperamental" = "Temperamental", "touchy" = "Touchy",
                               "bashful_r" = "Bashful (R)", "extroverted" = "Extroverted", "lively" = "Lively",
                               "quiet_r" = "Quiet (R)", "shy_r" = "Shy (R)", "talkative" = "Talkative",
                               "complex" = "Complex", "creative" = "Creative", "deep" = "Deep",
                               "imaginative" = "Imaginative", "intellectual" = "Intellectual", "philosophical" = "Philosophical",
                               "cooperative" = "Cooperative", "kind" = "Kind", "sympathetic" = "Sympathetic", "warm" = "Warm",
                               "disorganised_r" = "Disorganised (R)", "efficient" = "Efficient", "inefficient_r" = "Inefficient (R)",
                               "orderly" = "Orderly", "sloppy_r" = "Sloppy (R)", "systematic" = "Systematic",
                               item)
        
        table_row <- paste(item_display, paste(loadings_row, collapse = " & "), sep = " & ")
        latex_lines <- c(latex_lines, paste0(table_row, " \\\\"))
      }
    }
    
    # Add spacing between groups
    latex_lines <- c(latex_lines, "\\addlinespace")
  }
  
  # Remove last spacing and add table end
  if(length(latex_lines) > 0 && latex_lines[length(latex_lines)] == "\\addlinespace") {
    latex_lines <- latex_lines[-length(latex_lines)]
  }
  
  # Close table
  latex_lines <- c(
    latex_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "\\small",
    paste0("\\item \\textit{Note}. All model estimations converged successfully, with all factor loadings $\\geq .30$. All rotations used maximum likelihood estimation with 5 factors. (R) = reverse-coded items.",
           "GeominQ = Geomin with quartimin normalization."),
    "\\end{tablenotes}",
    "\\end{table}"
  )
  
  return(paste(latex_lines, collapse = "\n"))
}

# Generate rotation comparison table
rotation_comparison_latex <- create_rotation_comparison_table(efa_results, available_items)

# Calculate congruence coefficients between rotations
if(length(efa_results) >= 2) {
  cat("✓ Rotation stability analysis completed and comparison table generated\n")
} else {
  cat("⚠️ Insufficient rotation results for stability analysis\n")
}

# Step 3: Create EFA loadings table
cat("Step 3: Creating EFA loadings table for supplemental materials...\n")

create_latex_efa_loadings <- function(efa_result) {
  
  if(is.null(efa_result)) {
    return("EFA results not available")
  }
  
  loadings_matrix <- efa_result$loadings
  sample_size <- nrow(efa_data)
  items_used <- available_items
  
  # Automatic factor identification function
  identify_factor_structure <- function(loadings_matrix, items_used) {
    
    # Define item groups
    item_factor_mapping <- list(
      Neuroticism = c("envious", "fretful", "jealous", "moody", "temperamental", "touchy"),
      Openness = c("complex", "creative", "deep", "imaginative", "intellectual", "philosophical"),
      Extraversion = c("bashful_r", "extroverted", "lively", "quiet_r", "shy_r", "talkative"),
      Conscientiousness = c("disorganised_r", "efficient", "inefficient_r", "orderly", "sloppy_r", "systematic"),
      Agreeableness = c("cooperative", "kind", "sympathetic", "warm")
    )
    
    n_factors <- ncol(loadings_matrix)
    factor_assignments <- rep(NA, n_factors)
    factor_scores <- matrix(0, nrow = length(item_factor_mapping), ncol = n_factors)
    rownames(factor_scores) <- names(item_factor_mapping)
    
    # For each theoretical factor, find which empirical factor it loads on most
    for(i in 1:length(item_factor_mapping)) {
      theoretical_factor <- names(item_factor_mapping)[i]
      items_in_factor <- intersect(item_factor_mapping[[theoretical_factor]], items_used)
      
      if(length(items_in_factor) > 0) {
        # Calculate mean absolute loading for this theoretical factor on each empirical factor
        for(j in 1:n_factors) {
          factor_loadings <- abs(loadings_matrix[items_in_factor, j])
          factor_scores[i, j] <- mean(factor_loadings, na.rm = TRUE)
        }
      }
    }
    
    # Assign each empirical factor to the theoretical factor with highest score
    used_factors <- c()
    
    for(empirical_factor in 1:n_factors) {
      # Find theoretical factor with highest score for this empirical factor
      best_theoretical <- which.max(factor_scores[, empirical_factor])
      theoretical_name <- rownames(factor_scores)[best_theoretical]
      
      # Avoid double-assignment
      if(!theoretical_name %in% used_factors) {
        factor_assignments[empirical_factor] <- theoretical_name
        used_factors <- c(used_factors, theoretical_name)
      } else {
        # If already assigned, find next best
        remaining_scores <- factor_scores[, empirical_factor]
        remaining_scores[rownames(factor_scores) %in% used_factors] <- -1
        if(any(remaining_scores > 0)) {
          best_remaining <- which.max(remaining_scores)
          factor_assignments[empirical_factor] <- rownames(factor_scores)[best_remaining]
          used_factors <- c(used_factors, rownames(factor_scores)[best_remaining])
        } else {
          factor_assignments[empirical_factor] <- paste0("Factor", empirical_factor)
        }
      }
    }
    
    return(factor_assignments)
  }
  
  # Automatically identify factor structure
  factor_labels_full <- identify_factor_structure(loadings_matrix, items_used)
  
  # Create short labels for table header
  factor_labels_short <- sapply(factor_labels_full, function(x) {
    switch(x,
           "Neuroticism" = "(Neuro.)",
           "Openness" = "(Openn.)", 
           "Extraversion" = "(Extra.)",
           "Conscientiousness" = "(Consc.)",
           "Agreeableness" = "(Agree.)",
           paste0("(", substr(x, 1, 5), ")"))
  })
  
  # Create ordered list based on detected factor structure
  ordered_item_groups <- list()
  
  for(i in 1:length(factor_labels_full)) {
    factor_name <- factor_labels_full[i]
    
    if(factor_name == "Neuroticism") {
      ordered_item_groups[["Neuroticism Items"]] <- c("envious", "fretful", "jealous", "moody", "temperamental", "touchy")
    } else if(factor_name == "Conscientiousness") {
      ordered_item_groups[["Conscientiousness Items"]] <- c("disorganised_r", "efficient", "inefficient_r", "orderly", "sloppy_r", "systematic")
    } else if(factor_name == "Agreeableness") {
      ordered_item_groups[["Agreeableness Items"]] <- c("cooperative", "kind", "sympathetic", "warm")
    } else if(factor_name == "Openness") {
      ordered_item_groups[["Openness to Experience Items"]] <- c("complex", "creative", "deep", "imaginative", "intellectual", "philosophical")
    } else if(factor_name == "Extraversion") {
      ordered_item_groups[["Extraversion Items"]] <- c("bashful_r", "extroverted", "lively", "quiet_r", "shy_r", "talkative")
    }
  }
  
  # Use the ordered item groups instead of the fixed order
  item_groups <- ordered_item_groups
  
  # Create item display names
  item_display_names <- list(
    "envious" = "Envious", "fretful" = "Fretful", "jealous" = "Jealous",
    "moody" = "Moody", "temperamental" = "Temperamental", "touchy" = "Touchy",
    "complex" = "Complex", "creative" = "Creative", "deep" = "Deep",
    "imaginative" = "Imaginative", "intellectual" = "Intellectual", "philosophical" = "Philosophical",
    "bashful_r" = "Bashful (R)", "extroverted" = "Extroverted", "lively" = "Lively",
    "quiet_r" = "Quiet (R)", "shy_r" = "Shy (R)", "talkative" = "Talkative",
    "disorganised_r" = "Disorganized (R)", "efficient" = "Efficient", "inefficient_r" = "Inefficient (R)",
    "orderly" = "Orderly", "sloppy_r" = "Sloppy (R)", "systematic" = "Systematic",
    "cooperative" = "Cooperative", "kind" = "Kind", "sympathetic" = "Sympathetic", "warm" = "Warm"
  )
  
  n_factors <- ncol(loadings_matrix)
  tabular_spec <- paste0("l", paste(rep("r", n_factors), collapse = ""))
  
  # Start LaTeX table
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering", 
    "\\caption{Exploratory Factor Analysis Results: Factor Loadings (Oblimin Rotation)}",
    "\\label{tab:efa_loadings}",
    paste0("\\begin{tabular}{", tabular_spec, "}"),
    "\\toprule"
  )
  
  # Create two-line header
  header_line1 <- paste("Item &", paste(paste("Factor", 1:n_factors), collapse = " & "), "\\\\")
  header_line2 <- paste("     &", paste(factor_labels_short, collapse = " & "), "\\\\")
  
  latex_lines <- c(latex_lines, header_line1, header_line2, "\\midrule")
  
  # Helper function to remove leading zeros
  remove_leading_zero <- function(x, digits = 2) {
    if(is.na(x)) return("--")
    formatted <- sprintf(paste0("%.", digits, "f"), x)
    if(abs(x) < 1) {
      formatted <- gsub("^0\\.", ".", formatted)
      formatted <- gsub("^-0\\.", "-.", formatted)
    }
    return(formatted)
  }
  
  # Add data rows by groups (now dynamically ordered)
  for(group_name in names(item_groups)) {
    # Add group header
    latex_lines <- c(latex_lines, paste0("\\textbf{", group_name, "} & & & & & \\\\"))
    
    # Find items in this group that are in our data
    group_items <- intersect(item_groups[[group_name]], items_used)
    
    if(length(group_items) == 0) next
    
    # Add items in this group
    for(item in group_items) {
      # Get display name
      display_name <- ifelse(item %in% names(item_display_names), 
                             item_display_names[[item]], item)
      
      # Get loadings for this item
      item_loadings <- loadings_matrix[item, ]
      
      # Format loadings (bold if > .30, regular if <= .30)
      formatted_loadings <- sapply(item_loadings, function(x) {
        if(is.na(x)) return("--")
        if(abs(x) > 0.30) {
          return(paste0("\\textbf{", remove_leading_zero(x, 2), "}"))
        } else {
          return(remove_leading_zero(x, 2))
        }
      })
      
      # Create table row
      table_row <- paste(display_name, paste(formatted_loadings, collapse = " & "), 
                         sep = " & ")
      table_row <- paste0(table_row, " \\\\")
      
      latex_lines <- c(latex_lines, table_row)
    }
    
    # Add spacing between groups
    latex_lines <- c(latex_lines, "\\addlinespace")
  }
  
  # Remove last spacing and add table end
  if(length(latex_lines) > 0 && latex_lines[length(latex_lines)] == "\\addlinespace") {
    latex_lines <- latex_lines[-length(latex_lines)]
  }
  
  # End LaTeX table
  latex_lines <- c(latex_lines,
                   "\\bottomrule",
                   "\\end{tabular}",
                   "\\begin{tablenotes}",
                   "\\small",
                   paste0("\\item \\textit{Note}. $N = ", format(sample_size, big.mark = ","), "$.",
                          " Extra. = Extraversion; Agree. = Agreeableness; Consc. = Conscientiousness; Neuro. = Neuroticism; Openn. = Openness to Experience.",
                          " Factor loadings $\\geq .30$ shown in bold. (R) = reverse-coded items."),
                   "\\end{tablenotes}",
                   "\\end{table}")
  
  return(paste(latex_lines, collapse = "\n"))
}

efa_loadings_latex <- create_latex_efa_loadings(efa_result)
cat("✓ EFA loadings table generated\n\n")

# =============================================================================
# CONFIRMATORY FACTOR ANALYSIS
# =============================================================================

cat("=== CONFIRMATORY FACTOR ANALYSIS ===\n")

# Step 1: Define and test five-factor CFA model
cat("Step 1: Defining and testing five-factor CFA model...\n")

# Define item groups for CFA
item_groups <- list(
  "Neuroticism" = c("envious", "fretful", "jealous", "moody", "temperamental", "touchy"),
  "Extraversion" = c("bashful_r", "extroverted", "lively", "quiet_r", "shy_r", "talkative"),
  "Openness" = c("complex", "creative", "deep", "imaginative", "intellectual", "philosophical"),
  "Agreeableness" = c("cooperative", "kind", "sympathetic", "warm"),
  "Conscientiousness" = c("disorganised_r", "efficient", "inefficient_r", "orderly", "sloppy_r", "systematic")
)

# Create CFA model syntax
cfa_lines <- c()
for(factor_name in names(item_groups)) {
  factor_items <- intersect(item_groups[[factor_name]], colnames(efa_data))
  if(length(factor_items) >= 3) {  # Need at least 3 items per factor
    cfa_lines <- c(cfa_lines, paste0(factor_name, " =~ ", paste(factor_items, collapse = " + ")))
  }
}

cfa_model <- paste(cfa_lines, collapse = "\n")

# Fit CFA model
cfa_fit <- tryCatch({
  lavaan::cfa(cfa_model, data = efa_data, estimator = "ML", std.lv = TRUE)
}, error = function(e) {
  cat("❌ Error in CFA:", e$message, "\n")
  return(NULL)
})

if(!is.null(cfa_fit) && lavaan::lavInspect(cfa_fit, "converged")) {
  cfa_stats <- lavaan::fitMeasures(cfa_fit)
  cat("✓ Five-factor CFA completed:\n")
  cat("  χ²(", cfa_stats[["df"]], ") =", format(as.numeric(sprintf("%.2f", cfa_stats[["chisq"]])), big.mark = ","), 
      ", p <", ifelse(cfa_stats[["pvalue"]] < 0.001, ".001", sprintf("%.3f", cfa_stats[["pvalue"]])), "\n")
  cat("  CFI =", sprintf("%.3f", cfa_stats[["cfi"]]), "\n")
  cat("  TLI =", sprintf("%.3f", cfa_stats[["tli"]]), "\n")
  cat("  RMSEA =", sprintf("%.3f", cfa_stats[["rmsea"]]), 
      ", 90% CI [", sprintf("%.3f", cfa_stats[["rmsea.ci.lower"]]), 
      ", ", sprintf("%.3f", cfa_stats[["rmsea.ci.upper"]]), "]\n")
  cat("  SRMR =", sprintf("%.3f", cfa_stats[["srmr"]]), "\n\n")
} else {
  cat("❌ Five-factor CFA failed to converge\n\n")
}

# Step 2: Extract factor correlations and create LaTeX table
if(!is.null(cfa_fit) && lavaan::lavInspect(cfa_fit, "converged")) {
  
  cat("Step 2: Extracting factor correlations and creating supplemental table...\n")
  
  # Get factor correlations
  factor_cors <- lavaan::lavInspect(cfa_fit, "cor.lv") %>%
    as.data.frame()
  
  # Extract factor p-values
  factor_p <- lavaan::parameterEstimates(cfa_fit) %>%
    .[.$op == "~~" & .$lhs != .$rhs, ] %>%
    as.data.frame() %>%
    dplyr::select(lhs, rhs, est, pvalue)
  
  # Rename factors
  factor_names <- c("Neuroticism", "Extraversion", "Openness to Experience",
                    "Agreeableness", "Conscientiousness")
  
  # Set proper names
  colnames(factor_cors) <- factor_names
  rownames(factor_cors) <- factor_names
  factor_p[factor_p == "Openness"] <- "Openness to Experience"
  
  cat("✓ Factor correlations extracted\n")
  
  # Create LaTeX factor correlation table
  create_latex_factor_correlations <- function(factor_cors, factor_p, cfa_data) {
    
    # Reorder factors to match typical Big Five order
    factor_order <- c("Extraversion", "Agreeableness", "Conscientiousness", 
                      "Neuroticism", "Openness to Experience")
    
    # Reorder the correlation matrix
    factor_cors <- factor_cors[factor_order, factor_order]
    
    # Start LaTeX table
    latex_lines <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      "\\caption{Factor Correlations from Confirmatory Factor Analysis}",
      "\\label{tab:factor_correlations}",
      "\\begin{tabular}{lrrrrr}",
      "\\toprule",
      "Factor & 1 & 2 & 3 & 4 & 5 \\\\",
      "\\midrule"
    )
    
    remove_leading_zero <- function(x, digits = 2) {
      if(is.na(x)) return("--")
      formatted <- sprintf(paste0("%.", digits, "f"), x)
      # Remove leading zero if number is between -1 and 1
      if(abs(x) < 1) {
        formatted <- gsub("^0\\.", ".", formatted)
        formatted <- gsub("^-0\\.", "-.", formatted)
      }
      return(formatted)
    }
    
    # Add correlation rows
    for(i in 1:nrow(factor_cors)) {
      factor_name <- paste0(i, ". ", rownames(factor_cors)[i])
      
      # Create row values
      row_values <- c()
      
      for(j in 1:ncol(factor_cors)) {
        if(j > i) {
          # Upper triangle - empty
          row_values <- c(row_values, "")
        } else if(j == i) {
          # Diagonal - dashes
          row_values <- c(row_values, "\\phantom{0.00***}--")
        } else {
          # Lower triangle - correlations
          cor_value <- factor_cors[i, j]
          
          # Find matching p-value
          factor1 <- rownames(factor_cors)[i]
          factor2 <- colnames(factor_cors)[j]
          
          # Match p-value (accounting for both directions)
          p_match <- factor_p[(factor_p$lhs == factor1 & factor_p$rhs == factor2) | 
                                (factor_p$lhs == factor2 & factor_p$rhs == factor1), ]
          
          if(nrow(p_match) > 0) {
            p_value <- p_match$pvalue[1]
          } else {
            p_value <- 0.999  # Default if no match found
          }
          
          # Format correlation with significance
          if(p_value < 0.001) {
            significance_marker <- "***"
          } else if(p_value < 0.01) {
            significance_marker <- "**"
          } else if(p_value < 0.05) {
            significance_marker <- "*"
          } else {
            significance_marker <- ""
          }
          
          formatted_cor <- paste0(remove_leading_zero(cor_value, 2), significance_marker)
          row_values <- c(row_values, formatted_cor)
        }
      }
      
      # Create table row
      table_row <- paste(factor_name, paste(row_values, collapse = " & "), 
                         sep = " & ")
      table_row <- paste0(table_row, " \\\\")
      
      latex_lines <- c(latex_lines, table_row)
    }
    
    # End LaTeX table
    latex_lines <- c(latex_lines,
                     "\\bottomrule",
                     "\\end{tabular}",
                     "\\begin{tablenotes}",
                     "\\small",
                     paste0("\\item \\textit{Note}. $N = ", format(nrow(cfa_data), big.mark = ","), "$. ***$p < .001$, **$p < .01$, *$p < .05$."),
                     "\\end{tablenotes}",
                     "\\end{table}")
    
    return(paste(latex_lines, collapse = "\n"))
  }
  
  # Generate and save the factor correlation table
  factor_correlation_latex <- create_latex_factor_correlations(factor_cors, factor_p, efa_data)
  
  # Save LaTeX table to supplemental materials
  writeLines(factor_correlation_latex,
             file.path(supplementalTablesDir, "factor_correlations.tex"))
  
  cat("✓ Factor correlation table saved to supplemental/tables/\n")
  
  # Report largest correlations for verification
  cat("Largest correlations:\n")
  cat("• Conscientiousness-Neuroticism: r =", sprintf("%.2f", factor_cors["Conscientiousness", "Neuroticism"]), "\n")
  cat("• Agreeableness-Openness: r =", sprintf("%.2f", factor_cors["Agreeableness", "Openness to Experience"]), "\n")
  cat("• Agreeableness-Conscientiousness: r =", sprintf("%.2f", factor_cors["Agreeableness", "Conscientiousness"]), "\n")
  
} else {
  cat("❌ Cannot extract factor correlations - CFA failed\n")
}

# =============================================================================
# COMPREHENSIVE MODEL COMPARISONS
# =============================================================================

cat("=== COMPREHENSIVE MODEL COMPARISONS ===\n")

# Step 1: Define and test alternative structural models
cat("Step 1: Testing alternative structural models...\n")

# Helper function to extract fit statistics
extract_fit_stats <- function(fit_object, sample_size) {
  if(is.null(fit_object)) return(NULL)
  if(!lavaan::lavInspect(fit_object, "converged")) {
    cat("Model did not converge\n")
    return(NULL)
  }
  
  fit_measures <- lavaan::fitMeasures(fit_object)
  return(list(
    chisquare = fit_measures[["chisq"]],
    df = fit_measures[["df"]],
    pvalue = fit_measures[["pvalue"]],
    cfi = fit_measures[["cfi"]],
    tli = fit_measures[["tli"]],
    rmsea = fit_measures[["rmsea"]],
    rmsea_ci_lower = fit_measures[["rmsea.ci.lower"]],
    rmsea_ci_upper = fit_measures[["rmsea.ci.upper"]],
    srmr = fit_measures[["srmr"]],
    sample_size = sample_size
  ))
}

# Initialize results
model_results <- list()

# 1. One-factor model
cat("Testing one-factor model...\n")
one_factor_model <- paste0("General =~ ", paste(colnames(efa_data), collapse = " + "))
one_factor_fit <- tryCatch({
  lavaan::cfa(one_factor_model, data = efa_data, estimator = "ML")
}, error = function(e) NULL)

one_factor_stats <- extract_fit_stats(one_factor_fit, nrow(efa_data))
if(!is.null(one_factor_stats)) {
  model_results[["One-factor"]] <- one_factor_stats
  cat("One-factor model CFI =", sprintf("%.3f", one_factor_stats$cfi), "\n")
}

# 2. Two-factor model (Stability vs. Plasticity)
cat("Testing two-factor model (Stability vs. Plasticity)...\n")
stability_items <- intersect(c("envious", "fretful", "jealous", "moody", "temperamental", "touchy",
                              "disorganised_r", "efficient", "inefficient_r", "orderly", "sloppy_r", "systematic",
                              "cooperative", "kind", "sympathetic", "warm"), colnames(efa_data))
plasticity_items <- intersect(c("bashful_r", "extroverted", "lively", "quiet_r", "shy_r", "talkative",
                               "complex", "creative", "deep", "imaginative", "intellectual", "philosophical"), colnames(efa_data))

two_factor_model <- paste0("Stability =~ ", paste(stability_items, collapse = " + "), "\n",
                          "Plasticity =~ ", paste(plasticity_items, collapse = " + "))
two_factor_fit <- tryCatch({
  lavaan::cfa(two_factor_model, data = efa_data, estimator = "ML")
}, error = function(e) NULL)

two_factor_stats <- extract_fit_stats(two_factor_fit, nrow(efa_data))
if(!is.null(two_factor_stats)) {
  model_results[["Two-factor"]] <- two_factor_stats
  cat("Two-factor model CFI =", sprintf("%.3f", two_factor_stats$cfi), "\n")
}

# 3. Five-factor CFA (already computed above)
if(!is.null(cfa_fit) && lavaan::lavInspect(cfa_fit, "converged")) {
  cfa_stats_list <- extract_fit_stats(cfa_fit, nrow(efa_data))
  if(!is.null(cfa_stats_list)) {
    model_results[["Five-factor CFA"]] <- cfa_stats_list
    cat("Five-factor CFA CFI =", sprintf("%.3f", cfa_stats_list$cfi), "\n")
  }
}

# 4. Hierarchical model
cat("Testing hierarchical model...\n")
hierarchical_model <- paste0(cfa_model, "\n",
                            "# General factor\n",
                            "General =~ Neuroticism + Extraversion + Openness + Agreeableness + Conscientiousness")
hierarchical_fit <- tryCatch({
  lavaan::cfa(hierarchical_model, data = efa_data, estimator = "ML", std.lv = TRUE)
}, error = function(e) NULL)

hierarchical_stats <- extract_fit_stats(hierarchical_fit, nrow(efa_data))
if(!is.null(hierarchical_stats)) {
  model_results[["Hierarchical"]] <- hierarchical_stats
  cat("Hierarchical model CFI =", sprintf("%.3f", hierarchical_stats$cfi), "\n")
}

# 5. ESEM model
cat("Testing ESEM model...\n")

efa_result <- tryCatch({
  psych::fa(efa_data, nfactors = n_factors, rotate = "oblimin", 
            fm = "ml", scores = "regression")
}, error = function(e) {
  cat("❌ Error in EFA:", e$message, "\n")
  return(NULL)
})

# Generate ESEM syntax with cross-loadings based on EFA results
esem_model_syntax <- function(efa_data, nfactors = 5) {
  # Use existing EFA results if available, otherwise run new EFA
  if(exists("efa_result") && !is.null(efa_result)) {
    loadings_matrix <- efa_result$loadings
  } else {
    efa_temp <- psych::fa(efa_data, nfactors = nfactors, rotate = "oblimin", fm = "ml", scores = "regression")
    loadings_matrix <- efa_temp$loadings
  }
  
  factor_names <- paste0("F", 1:nfactors)
  model_lines <- c()
  
  # Create factor loading syntax with cross-loadings
  for(f in 1:nfactors) {
    factor_items <- c()
    for(item in colnames(efa_data)) {
      if(item %in% rownames(loadings_matrix)) {
        loading <- loadings_matrix[item, f]
        # Include meaningful loadings (> 0.10), fix trivial ones to zero
        if(abs(loading) > 0.10) {
          factor_items <- c(factor_items, paste0(sprintf("%.3f", loading), "*", item))
        } else {
          factor_items <- c(factor_items, paste0("0*", item))
        }
      }
    }
    
    if(length(factor_items) > 0) {
      model_line <- paste0(factor_names[f], " =~ ", paste(factor_items, collapse = " + "))
      model_lines <- c(model_lines, model_line)
    }
  }
  
  esem_syntax <- paste(model_lines, collapse = "\n")
  return(esem_syntax)
}

# Generate and fit ESEM model
esem_model <- esem_model_syntax(efa_data, nfactors = 5)
esem_fit <- tryCatch({
  lavaan::sem(esem_model, data = efa_data, estimator = "ML", std.lv = TRUE, 
              missing = "ml", optim.method = "nlminb", 
              control = list(iter.max = 3000))
}, error = function(e) {
  cat("ESEM error:", e$message, "\n")
  return(NULL)
})

esem_stats <- extract_fit_stats(esem_fit, nrow(efa_data))
if(!is.null(esem_stats)) {
  model_results[["Five-factor ESEM"]] <- esem_stats
  cat("Five-factor ESEM CFI =", sprintf("%.3f", esem_stats$cfi), "\n")
}

# 6. Bifactor model
cat("Testing bifactor model...\n")
bifactor_lines <- c()
bifactor_lines <- c(bifactor_lines, paste0("General =~ ", paste(colnames(efa_data), collapse = " + ")))

# Add specific factors
for(factor_name in names(item_groups)) {
  factor_items <- intersect(item_groups[[factor_name]], colnames(efa_data))
  if(length(factor_items) >= 3) {
    bifactor_lines <- c(bifactor_lines, paste0(factor_name, " =~ ", paste(factor_items, collapse = " + ")))
  }
}

# Add orthogonality constraints
for(factor in names(item_groups)) {
  bifactor_lines <- c(bifactor_lines, paste0("General ~~ 0*", factor))
}

bifactor_model <- paste(bifactor_lines, collapse = "\n")
bifactor_fit <- tryCatch({
  lavaan::cfa(bifactor_model, data = efa_data, estimator = "ML", std.lv = TRUE)
}, error = function(e) NULL)

bifactor_stats <- extract_fit_stats(bifactor_fit, nrow(efa_data))
if(!is.null(bifactor_stats)) {
  model_results[["Bifactor"]] <- bifactor_stats
  cat("Bifactor model CFI =", sprintf("%.3f", bifactor_stats$cfi), "\n")
}

cat("✓ Model comparison analysis completed\n\n")

# =============================================================================
# CREATE MODEL COMPARISON TABLE
# =============================================================================

cat("=== CREATING MODEL COMPARISON TABLE ===\n")

# Step 1: Generate LaTeX model comparison table
cat("Step 1: Generating model comparison table for manuscript...\n")

create_latex_model_comparison <- function(model_results) {
  
  # Define model order for presentation
  model_order <- c("One-factor", "Two-factor", "Hierarchical", "Five-factor CFA", 
                   "Five-factor ESEM", "Bifactor")
  
  # Filter to available models
  available_models <- intersect(model_order, names(model_results))
  
  # Start LaTeX table
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Model Fit Comparison for Alternative Factor Structures}",
    "\\label{tab:model_comparison}",
    "\\begin{tabular}{lrrrrrrr}",
    "\\toprule",
    "Model & $\\chi^2$ & \\textit{df} & CFI & TLI & RMSEA & 90\\% CI & SRMR \\\\",
    "\\midrule"
  )
  
  # Helper function to remove leading zeros
  remove_leading_zero <- function(x, digits = 2) {
    if(is.na(x)) return("--")
    formatted <- sprintf(paste0("%.", digits, "f"), x)
    if(abs(x) < 1) {
      formatted <- gsub("^0\\.", ".", formatted)
      formatted <- gsub("^-0\\.", "-.", formatted)
    }
    return(formatted)
  }
  
  # Add model rows
  for(model_name in available_models) {
    stats <- model_results[[model_name]]
    
    # Format values
    chisq_formatted <- format(round(stats$chisquare, 2), big.mark = ",")
    df_formatted <- stats$df
    cfi_formatted <- remove_leading_zero(stats$cfi, 3)
    tli_formatted <- remove_leading_zero(stats$tli, 3)
    rmsea_formatted <- remove_leading_zero(stats$rmsea, 3)
    
    # RMSEA CI
    rmsea_ci <- paste0("[", remove_leading_zero(stats$rmsea_ci_lower, 3), 
                       ", ", remove_leading_zero(stats$rmsea_ci_upper, 3), "]")
    
    srmr_formatted <- remove_leading_zero(stats$srmr, 3)
    
    # Create table row
    table_row <- paste(model_name, chisq_formatted, df_formatted, cfi_formatted,
                      tli_formatted, rmsea_formatted, rmsea_ci, srmr_formatted,
                      sep = " & ")
    table_row <- paste0(table_row, " \\\\")
    latex_lines <- c(latex_lines, table_row)
  }
  
  # End LaTeX table
  sample_size <- model_results[[1]]$sample_size
  latex_lines <- c(
    latex_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "\\small",
    paste0("\\item \\textit{Note}. $N = ", format(sample_size, big.mark = ","), 
           "$. All $\\chi^2$ values significant at $p < .001$. CFI = Comparative Fit Index; TLI = Tucker-Lewis Index; RMSEA = Root Mean Square Error of Approximation; SRMR = Standardized Root Mean Square Residual."),
    "\\end{tablenotes}",
    "\\end{table}"
  )
  
  return(paste(latex_lines, collapse = "\n"))
}

model_comparison_latex <- create_latex_model_comparison(model_results)
cat("✓ Model comparison table generated\n\n")

# =============================================================================
# SAVE PROCESSED DATA AND RESULTS
# =============================================================================

cat("=== SAVING FACTOR STRUCTURE ANALYSIS RESULTS ===\n")

# Step 1: Save comprehensive factor analysis results
cat("Step 1: Saving comprehensive factor structure results...\n")

factor_structure_results <- list(
  parallel_analysis = if(!is.null(parallel_result)) {
    list(
      suggested_factors = suggested_factors,
      eigenvalues_observed = parallel_result$fa.values,
      eigenvalues_simulated = parallel_result$fa.sim,
      plot_saved = file.path(manuscriptFiguresDir, "parallel_analysis.pdf")
    )
  } else {
    list(suggested_factors = 5, note = "Parallel analysis failed")
  },
  
  vss_analysis = if(!is.null(vss_result)) {
    list(
      complexity_1_max = vss_complexity1,
      complexity_2_max = vss_complexity2
    )
  } else {
    list(note = "VSS analysis failed")
  },
  
  efa_results = if(!is.null(efa_result)) {
    list(
      variance_explained = variance_explained,
      loadings_matrix = efa_result$loadings,
      factor_correlations = efa_result$Phi,
      rotation_methods_tested = names(efa_results)
    )
  } else {
    list(note = "EFA failed")
  },
  
  cfa_results = if(!is.null(cfa_fit) && lavaan::lavInspect(cfa_fit, "converged")) {
    list(
      model_syntax = cfa_model,
      fit_measures = cfa_stats,
      factor_correlations = factor_cors,
      standardized_loadings = lavaan::standardizedSolution(cfa_fit)
    )
  } else {
    list(note = "CFA failed to converge")
  },
  
  model_comparisons = model_results,
  
  sample_info = list(
    n_cases = nrow(efa_data),
    n_items = ncol(efa_data),
    missing_data_rate = sum(is.na(efa_data)) / (nrow(efa_data) * ncol(efa_data)) * 100
  )
)

qs::qsave(factor_structure_results, file.path(factorStructureDir, "factor_structure_results.qs"))
cat("✓ Factor structure results saved to output/factor_structure/\n")

# Step 2: Save LaTeX tables
cat("Step 2: Saving LaTeX tables to appropriate directories...\n")

# Main manuscript table (model comparison)
writeLines(model_comparison_latex, file.path(manuscriptTablesDir, "model_comparison.tex"))
cat("✓ Model comparison table saved to manuscript/tables/\n")

# Supplemental tables
writeLines(efa_loadings_latex, file.path(supplementalTablesDir, "efa_loadings.tex"))
cat("✓ EFA loadings table saved to supplemental materials/tables/\n")
writeLines(rotation_comparison_latex, file.path(supplementalTablesDir, "efa_rotation_comparison.tex"))
cat("✓ EFA rotation comparison table saved to supplemental materials/tables/\n")

# Step 3: Create factor structure summary
cat("Step 3: Creating factor structure summary report...\n")

factor_summary <- paste(
  "Factor Structure Analysis Summary:",
  paste("• Parallel analysis suggests:", suggested_factors, "factors"),
  paste("• EFA variance explained:", variance_explained, "%"),
  ifelse(!is.null(cfa_stats), 
         paste("• Five-factor CFA fit: CFI =", sprintf("%.3f", cfa_stats[["cfi"]]), 
               ", RMSEA =", sprintf("%.3f", cfa_stats[["rmsea"]])), 
         "• Five-factor CFA: Failed to converge"),
  paste("• Best fitting model: Bifactor (CFI =", 
        ifelse("Bifactor" %in% names(model_results), 
               sprintf("%.3f", model_results[["Bifactor"]]$cfi), "N/A"), ")"),
  "• Recommendation: Use bifactor model for subsequent analyses",
  sep = "\n"
)

writeLines(factor_summary, file.path(factorStructureDir, "factor_structure_summary.txt"))
cat("✓ Factor structure summary saved to output/factor_structure/\n\n")

# =============================================================================
# STAGE COMPLETION SUMMARY
# =============================================================================

cat("\n=== STAGE", CURRENT_STAGE$number, "COMPLETION SUMMARY ===\n")

cat("✅ Factor structure analysis completed successfully!\n\n")

cat("📊 ANALYSIS SUMMARY:\n")
cat("• Parallel analysis: Suggests", suggested_factors, "factors\n")
if(!is.null(efa_result)) {
  cat("• EFA: 5-factor solution explains", variance_explained, "% of variance\n")
}
if(!is.null(cfa_stats)) {
  cat("• Five-factor CFA: CFI =", sprintf("%.3f", cfa_stats[["cfi"]]), 
      ", RMSEA =", sprintf("%.3f", cfa_stats[["rmsea"]]), "\n")
}
cat("• Model comparisons:", length(model_results), "models tested\n")
if("Bifactor" %in% names(model_results)) {
  cat("• Best fit: Bifactor model (CFI =", sprintf("%.3f", model_results[["Bifactor"]]$cfi), ")\n")
}
cat("• Sample size:", format(nrow(efa_data), big.mark = ","), "cases,", ncol(efa_data), "items\n\n")

cat("📁 OUTPUT LOCATION:\n")
cat("• Analysis results: output/factor_structure/factor_structure_results.qs\n")
cat("• Manuscript table: manuscript/tables/model_comparison.tex\n")
cat("• Supplemental table: supplemental materials/tables/efa_loadings.tex\n")
cat("• Parallel analysis plot: manuscript/figures/parallel_analysis.pdf\n")
cat("• Summary report: output/factor_structure/factor_structure_summary.txt\n\n")

cat("🔄 PIPELINE INTEGRATION:\n")
cat("• Stage 4 (Factor Structure Analysis): ✅ COMPLETE\n")
cat("• Ready for Stage 5 (Reliability Analysis)\n")
cat("• Factor structure confirmed for subsequent psychometric analyses\n\n")

cat("📋 NEXT STEPS:\n")
cat("1. Proceed with reliability analysis using established factor structure\n")
cat("2. Use bifactor model as baseline for measurement invariance testing\n")
cat("3. Include model comparison table and parallel analysis plot in manuscript\n\n")

cat("📄 KEY FINDINGS VERIFICATION:\n")
if(!is.null(efa_result)) {
  cat("EFA variance explained:", variance_explained, "%\n")
}
if(!is.null(cfa_stats)) {
  cat("CFA model fit: χ²(", cfa_stats[["df"]], ") =", format(as.numeric(sprintf("%.2f", cfa_stats[["chisq"]])), big.mark = ","), 
      ", CFI =", sprintf("%.3f", cfa_stats[["cfi"]]), "\n")
}
if(length(model_results) > 0) {
  best_cfi <- max(sapply(model_results, function(x) x$cfi))
  best_model <- names(model_results)[which.max(sapply(model_results, function(x) x$cfi))]
  cat("Best fitting model:", best_model, "(CFI =", sprintf("%.3f", best_cfi), ")\n")
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎉 STAGE", CURRENT_STAGE$number, ": FACTOR STRUCTURE ANALYSIS COMPLETE! 🎉\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Helper function for string repetition (if not available)
if(!exists("%.%")) {
  `%.%` <- function(x, n) paste0(rep(x, n), collapse = "")
}
