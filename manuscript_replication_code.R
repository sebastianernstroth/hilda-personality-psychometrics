# =============================================================================
# HILDA PERSONALITY VALIDATION - MANUSCRIPT-BASED ANALYSIS PIPELINE
# =============================================================================
# 
# This script replicates the complete analysis pipeline following the manuscript structure:
# 1. Data Preparation (HILDA dataset preparation)
# 2. Participants and Measures (Method section)
# 3. Preliminary Analyses (missing data, descriptives)
# 4. Factor Structure Analysis (EFA, CFA, ESEM, model comparison)
# 5. Reliability Analysis (internal consistency, test-retest)
# 6. Measurement Invariance (longitudinal, multi-group)
# 7. Validity Analysis (criterion, discriminant, convergent)
# 8. Robustness Analysis (cross-validation, outliers, alternative methods)
# 
# Follows the exact sequence and methodology from Manuscript.tex
# =============================================================================

# Clear environment
rm(list = ls(all = TRUE))
graphics.off()

cat("=== HILDA PERSONALITY VALIDATION STUDY - MANUSCRIPT REPLICATION PIPELINE ===\n")
cat("Following manuscript structure: Longitudinal measurement invariance and stability of Big Five personality traits\n\n")

# =============================================================================
# PIPELINE CONFIGURATION
# =============================================================================

# Analysis configuration following manuscript methodology
MANUSCRIPT_PIPELINE <- list(
  # Script sequence (following manuscript Results section)
  scripts = list(
    data_preparation = "scripts/prepare_hilda_data.R",
    participants_measures = "scripts/participants_and_measures.R",
    preliminary_analyses = "scripts/preliminary_analyses.R",
    factor_structure = "scripts/factor_structure_analysis.R",
    reliability = "scripts/reliability_analysis.R",
    measurement_invariance = "scripts/measurement_invariance_analysis.R",
    validity = "scripts/validity_analysis.R",
    robustness = "scripts/robustness_analysis.R"
  ),
  
  # Execution options
  execution = list(
    run_data_preparation = TRUE,
    run_participants_measures = TRUE,
    run_preliminary_analyses = TRUE,
    run_factor_structure = TRUE,
    run_reliability = TRUE,
    run_measurement_invariance = TRUE,
    run_validity = TRUE,
    run_robustness = TRUE
  ),
  
  # Output organization (manuscript structure)
  output = list(
    create_manuscript_structure = TRUE,
    create_supplemental_structure = TRUE,
    organize_by_analysis_type = TRUE
  ),
  
  # Reproducibility settings
  reproducibility = list(
    set_seed = 12345,
    r_version_required = "4.5.1",
    save_session_info = TRUE,
    track_computational_time = TRUE
  )
)

# Set seed for reproducibility (as mentioned in manuscript)
set.seed(MANUSCRIPT_PIPELINE$reproducibility$set_seed)

# =============================================================================
# PROJECT STRUCTURE SETUP (MANUSCRIPT-BASED ORGANIZATION)
# =============================================================================

cat("=== SETTING UP MANUSCRIPT-BASED PROJECT STRUCTURE ===\n")

# Core directories
if(file.exists("hilda-personality-psychometrics.Rproj")) {
  # If RStudio project exists, use that directory
  root <- rprojroot::find_root(rprojroot::is_rstudio_project)
} else {
  # Otherwise use current working directory
  root <- getwd()
}
dataDir <- file.path(root, "data")
outputDir <- file.path(root, "output")

# Create manuscript structure (following journal requirements)
if(MANUSCRIPT_PIPELINE$output$create_manuscript_structure) {
  
  manuscript_structure <- list(
    # Main manuscript materials
    manuscript = file.path(root, "manuscript"),
    manuscript_tables = file.path(root, "manuscript", "tables"),
    manuscript_figures = file.path(root, "manuscript", "figures"),
    
    # Supplemental materials (extensive per manuscript)
    supplemental = file.path(root, "supplemental materials"),
    supplemental_tables = file.path(root, "supplemental materials", "tables"),
    supplemental_figures = file.path(root, "supplemental materials", "figures"),
    
    # Analysis-specific outputs
    participants = file.path(outputDir, "participants"),
    preliminary = file.path(outputDir, "preliminary"),
    factor_structure = file.path(outputDir, "factor_structure"),
    reliability = file.path(outputDir, "reliability"),
    measurement_invariance = file.path(outputDir, "measurement_invariance"),
    validity = file.path(outputDir, "validity"),
    robustness = file.path(outputDir, "robustness"),
    
    # Data organization
    raw_data = file.path(dataDir, "raw"),
    processed_data = file.path(outputDir, "data"),
    
    # Reproducibility
    environment = file.path(root, "environment"),
    logs = file.path(outputDir, "logs")
  )
  
  # Create all directories
  for(dir_path in manuscript_structure) {
    if(!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cat("Created:", basename(dir_path), "directory\n")
    }
  }
  
  cat("✓ Manuscript structure created\n\n")
}

# =============================================================================
# ANALYSIS EXECUTION FUNCTIONS
# =============================================================================

# Enhanced script execution with manuscript-specific tracking
execute_analysis_stage <- function(script_name, stage_description, stage_number, required = TRUE) {
  
  stage_header <- sprintf("=== STAGE %d: %s ===", stage_number, toupper(stage_description))
  cat(stage_header, "\n")
  
  stage_start <- Sys.time()
  
  if(file.exists(script_name)) {
    cat("Executing:", script_name, "\n")
    cat("Description:", stage_description, "\n")
    
    tryCatch({
      # Execute the analysis script
      source(script_name, echo = FALSE)
      
      stage_end <- Sys.time()
      stage_duration <- as.numeric(difftime(stage_end, stage_start, units = "secs"))
      
      cat("✅ Stage", stage_number, "completed successfully\n")
      cat("⏱️  Duration:", round(stage_duration, 1), "seconds\n\n")
      
      # Log successful execution
      execution_log[[paste0("stage_", stage_number)]] <<- list(
        stage = stage_number,
        description = stage_description,
        script = script_name,
        status = "SUCCESS",
        start_time = stage_start,
        end_time = stage_end,
        duration_seconds = stage_duration,
        error_message = NULL
      )
      
      return(TRUE)
      
    }, error = function(e) {
      stage_end <- Sys.time()
      stage_duration <- as.numeric(difftime(stage_end, stage_start, units = "secs"))
      
      cat("❌ ERROR in Stage", stage_number, ":", e$message, "\n")
      
      # Log error
      execution_log[[paste0("stage_", stage_number)]] <<- list(
        stage = stage_number,
        description = stage_description,
        script = script_name,
        status = "ERROR",
        start_time = stage_start,
        end_time = stage_end,
        duration_seconds = stage_duration,
        error_message = e$message
      )
      
      if(required) {
        stop("❌ Required analysis stage failed: Stage ", stage_number, " - ", stage_description)
      } else {
        cat("⚠️ Optional analysis stage failed, continuing pipeline...\n\n")
        return(FALSE)
      }
    })
    
  } else {
    cat("⚠️ Script not found:", script_name, "\n")
    
    if(required) {
      stop("❌ Required script not found: ", script_name)
    } else {
      cat("⚠️ Skipping optional stage\n\n")
      return(FALSE)
    }
  }
}

# =============================================================================
# MANUSCRIPT REPLICATION PIPELINE EXECUTION
# =============================================================================

# Initialize execution tracking
pipeline_start <- Sys.time()
execution_log <- list()
successful_stages <- 0
total_stages <- sum(unlist(MANUSCRIPT_PIPELINE$execution))

cat("Starting manuscript replication pipeline with", total_stages, "analysis stages\n\n")

# STAGE 1: DATA PREPARATION
if(MANUSCRIPT_PIPELINE$execution$run_data_preparation) {
  stage_1_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$data_preparation,
    "Data Preparation - HILDA Dataset Processing",
    stage_number = 1,
    required = FALSE  # Optional if data already exists
  )
  if(stage_1_success) successful_stages <- successful_stages + 1
}

# STAGE 2: PARTICIPANTS AND MEASURES (Method Section)
if(MANUSCRIPT_PIPELINE$execution$run_participants_measures) {
  stage_2_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$participants_measures,
    "Participants and Measures - Method Section Analysis",
    stage_number = 2,
    required = TRUE
  )
  if(stage_2_success) successful_stages <- successful_stages + 1
}

# STAGE 3: PRELIMINARY ANALYSES (Results Section Start)
if(MANUSCRIPT_PIPELINE$execution$run_preliminary_analyses) {
  stage_3_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$preliminary_analyses,
    "Preliminary Analyses - Missing Data & Descriptives",
    stage_number = 3,
    required = TRUE
  )
  if(stage_3_success) successful_stages <- successful_stages + 1
}

# STAGE 4: FACTOR STRUCTURE ANALYSIS (Core Results)
if(MANUSCRIPT_PIPELINE$execution$run_factor_structure) {
  stage_4_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$factor_structure,
    "Factor Structure Analysis - EFA, CFA, ESEM, Model Comparison",
    stage_number = 4,
    required = TRUE
  )
  if(stage_4_success) successful_stages <- successful_stages + 1
}

# STAGE 5: RELIABILITY ANALYSIS 
if(MANUSCRIPT_PIPELINE$execution$run_reliability) {
  stage_5_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$reliability,
    "Reliability Analysis - Internal Consistency & Test-Retest",
    stage_number = 5,
    required = TRUE
  )
  if(stage_5_success) successful_stages <- successful_stages + 1
}

# STAGE 6: MEASUREMENT INVARIANCE
if(MANUSCRIPT_PIPELINE$execution$run_measurement_invariance) {
  stage_6_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$measurement_invariance,
    "Measurement Invariance - Longitudinal & Multi-Group",
    stage_number = 6,
    required = TRUE
  )
  if(stage_6_success) successful_stages <- successful_stages + 1
}

# STAGE 7: VALIDITY ANALYSIS
if(MANUSCRIPT_PIPELINE$execution$run_validity) {
  stage_7_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$validity,
    "Validity Analysis - Criterion, Convergent, Discriminant",
    stage_number = 7,
    required = TRUE
  )
  if(stage_7_success) successful_stages <- successful_stages + 1
}

# STAGE 8: ROBUSTNESS ANALYSIS
if(MANUSCRIPT_PIPELINE$execution$run_robustness) {
  stage_8_success <- execute_analysis_stage(
    MANUSCRIPT_PIPELINE$scripts$robustness,
    "Robustness Analysis - Cross-validation, Outliers, Alternative Methods",
    stage_number = 8,
    required = FALSE  # Optional final checks
  )
  if(stage_8_success) successful_stages <- successful_stages + 1
}

# =============================================================================
# POST-ANALYSIS FILE ORGANIZATION
# =============================================================================

cat("=== ORGANIZING MANUSCRIPT OUTPUTS ===\n")

# Function to organize files according to manuscript structure
organize_manuscript_outputs <- function() {
  
  # Define manuscript-specific file organization
  manuscript_file_rules <- list(
    
    # MAIN MANUSCRIPT TABLES (Tables 1-7 in paper)
    main_tables = list(
      patterns = c("sample_characteristics.tex", "personality_items", "model_comparison",
                   "reliability_analysis", "longitudinal_invariance", "criterion_validity",
                   "demographic_differences"),
      destination = manuscript_structure$manuscript_tables,
      description = "Main manuscript tables"
    ),
    
    # SUPPLEMENTAL MATERIALS TABLES (Extensive supplemental materials per manuscript)
    supplemental_tables = list(
      patterns = c("descriptive_stats", "efa_loadings", "efa_rotation_comparison",
                   "factor_correlations", "test_retest", "temporal_stability_matrix", 
                  "multigroup_invariance", "robustness_summary.tex"),
      destination = manuscript_structure$supplemental_tables,
      description = "Supplemental materials tables"
    ),
    
    # MAIN MANUSCRIPT FIGURES (Figure 1 in paper) 
    main_figures = list(
      patterns = c("parallel_analysis"),
      destination = manuscript_structure$manuscript_figures,
      description = "Main manuscript figures"
    ),
    
    # ANALYSIS-SPECIFIC DATA AND RESULTS
    preliminary_outputs = list(
      patterns = c("missing_data_summary", "preliminary_results"),
      destination = manuscript_structure$preliminary,
      description = "Preliminary analysis outputs"
    ),
    
    factor_outputs = list(
      patterns = c("factor_structure_results", "factor_structure_summary"),
      destination = manuscript_structure$factor_structure,
      description = "Factor structure analysis outputs"
    ),
    
    reliability_outputs = list(
      patterns = c("reliability_results", "reliability_summary"),
      destination = manuscript_structure$reliability,
      description = "Reliability analysis outputs"
    ),
    
    invariance_outputs = list(
      patterns = c("measurement_invariance_results", "measurement_invariance_summary"),
      destination = manuscript_structure$measurement_invariance,
      description = "Measurement invariance outputs"
    ),
    
    validity_outputs = list(
      patterns = c("validity_results", "validity_summary"),
      destination = manuscript_structure$validity,
      description = "Validity analysis outputs"
    ),
    
    robustness_outputs = list(
      patterns = c("robustness_results", "robustness_summary.txt"),
      destination = manuscript_structure$robustness,
      description = "Robustness analysis outputs"
    )
  )
  
  # Get all output files
  all_output_files <- list.files(outputDir, pattern = "\\.(qs|tex|txt|pdf)$", 
                                full.names = TRUE, recursive = TRUE)
  
  files_organized <- 0
  
  # Organize files according to rules
  for(rule_name in names(manuscript_file_rules)) {
    rule <- manuscript_file_rules[[rule_name]]
    
    # Find files matching any of the patterns
    matching_files <- c()
    for(pattern in rule$patterns) {
      pattern_matches <- all_output_files[grepl(pattern, basename(all_output_files), ignore.case = TRUE)]
      matching_files <- c(matching_files, pattern_matches)
    }
    
    # Remove duplicates
    matching_files <- unique(matching_files)
    
    if(length(matching_files) > 0) {
      cat("Organizing", length(matching_files), rule$description, "\n")
      
      for(file_path in matching_files) {
        dest_path <- file.path(rule$destination, basename(file_path))
        
        # Only move if not already in destination
        if(normalizePath(dirname(file_path)) != normalizePath(rule$destination)) {
          tryCatch({
            file.copy(file_path, dest_path, overwrite = TRUE)
            file.remove(file_path)
            files_organized <- files_organized + 1
          }, error = function(e) {
            cat("  Warning: Could not organize", basename(file_path), "\n")
          })
        }
      }
    }
  }
  
  cat("✓ Organized", files_organized, "output files\n")
}

# Organize outputs if structure was created
if(MANUSCRIPT_PIPELINE$output$create_manuscript_structure) {
  organize_manuscript_outputs()
}

# =============================================================================
# COMPREHENSIVE SESSION DOCUMENTATION
# =============================================================================

cat("\n=== DOCUMENTING ANALYSIS SESSION ===\n")

# Calculate total pipeline time
pipeline_end <- Sys.time()
total_duration <- as.numeric(difftime(pipeline_end, pipeline_start, units = "mins"))

# Create comprehensive documentation (manuscript requirements)
session_documentation <- list(
  # Pipeline execution summary
  pipeline_summary = list(
    manuscript_title = "Longitudinal measurement invariance and stability of Big Five personality traits",
    analysis_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    total_duration_minutes = round(total_duration, 2),
    successful_stages = successful_stages,
    total_stages = total_stages,
    completion_rate = round(successful_stages / total_stages * 100, 1)
  ),
  
  # Detailed stage execution log
  execution_log = execution_log,
  
  # Computational environment (for reproducibility)
  computational_environment = list(
    r_version = R.version.string,
    platform = R.version$platform,
    operating_system = Sys.info()["sysname"],
    user = Sys.info()["user"],
    working_directory = getwd(),
    seed_used = MANUSCRIPT_PIPELINE$reproducibility$set_seed
  ),
  
  # Analysis configuration
  pipeline_configuration = MANUSCRIPT_PIPELINE,
  
  # R session information (detailed)
  r_session_info = sessionInfo()
)

# Save documentation in multiple formats
if(MANUSCRIPT_PIPELINE$reproducibility$save_session_info) {
  
  # Binary format for programmatic access
  qs::qsave(session_documentation, 
           file.path(manuscript_structure$environment, "session_documentation.qs"))
  
  # Human-readable format
  sink(file.path(manuscript_structure$environment, "pipeline_execution_log.txt"))
  
  cat("HILDA PERSONALITY VALIDATION STUDY - MANUSCRIPT REPLICATION PIPELINE\n")
  cat("=" %.% 70, "\n\n")
  
  cat("EXECUTION SUMMARY\n")
  cat("-" %.% 50, "\n")
  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total Duration:", round(total_duration, 2), "minutes\n")
  cat("Successful Stages:", successful_stages, "of", total_stages, 
      paste0("(", round(successful_stages/total_stages*100, 1), "%)\n"))
  cat("Seed Used:", MANUSCRIPT_PIPELINE$reproducibility$set_seed, "\n\n")
  
  cat("STAGE EXECUTION DETAILS\n")
  cat("-" %.% 50, "\n")
  for(stage_name in names(execution_log)) {
    stage_info <- execution_log[[stage_name]]
    cat("Stage", stage_info$stage, ":", stage_info$description, "\n")
    cat("  Status:", stage_info$status, "\n")
    cat("  Duration:", round(stage_info$duration_seconds, 1), "seconds\n")
    if(!is.null(stage_info$error_message)) {
      cat("  Error:", stage_info$error_message, "\n")
    }
    cat("\n")
  }
  
  cat("COMPUTATIONAL ENVIRONMENT\n")
  cat("-" %.% 50, "\n")
  print(sessionInfo())
  
  sink()
  
  cat("✓ Comprehensive session documentation saved\n")
}

# =============================================================================
# FINAL PIPELINE SUMMARY
# =============================================================================

cat("\n=== MANUSCRIPT REPLICATION PIPELINE COMPLETE ===\n")
cat("🎉 Analysis pipeline finished successfully!\n\n")

# Execution summary
cat("PIPELINE SUMMARY\n")
cat("📊 Total execution time:", round(total_duration, 2), "minutes\n")
cat("✅ Successfully completed:", successful_stages, "of", total_stages, "analysis stages\n")
cat("📈 Success rate:", round(successful_stages/total_stages*100, 1), "%\n\n")

# Stage completion overview
cat("COMPLETED ANALYSIS STAGES:\n")
stage_names <- c(
  "1. Data Preparation",
  "2. Participants and Measures", 
  "3. Preliminary Analyses",
  "4. Factor Structure Analysis",
  "5. Reliability Analysis",
  "6. Measurement Invariance", 
  "7. Validity Analysis",
  "8. Robustness Analysis"
)

for(i in 1:length(stage_names)) {
  stage_key <- paste0("stage_", i)
  if(stage_key %in% names(execution_log) && execution_log[[stage_key]]$status == "SUCCESS") {
    cat("  ✅", stage_names[i], "\n")
  } else if(stage_key %in% names(execution_log) && execution_log[[stage_key]]$status == "ERROR") {
    cat("  ❌", stage_names[i], "- FAILED\n")
  } else {
    cat("  ⚪", stage_names[i], "- SKIPPED\n")
  }
}

# Output organization summary
cat("\nOUTPUT ORGANIZATION:\n")
if(exists("manuscript_structure")) {
  cat("📁 Manuscript tables:", manuscript_structure$manuscript_tables, "\n")
  cat("📁 Supplemental tables:", manuscript_structure$supplemental_tables, "\n")
  cat("📁 Analysis outputs organized by stage in output/ subdirectories\n")
  cat("📁 Session documentation:", manuscript_structure$environment, "\n")
}

# Manuscript correspondence
cat("\nMANUSCRIPT CORRESPONDENCE:\n")
cat("📝 This pipeline replicates the exact analysis sequence from:\n")
cat("   'Longitudinal measurement invariance and stability of Big Five personality traits'\n")
cat("📊 Results should match manuscript Tables 1-7, Figure 1, and Supplemental Tables S1-S8\n")
cat("🔬 All analyses follow manuscript methodology using contemporary psychometric approaches\n")

# Reproducibility information
cat("\nREPRODUCIBILITY:\n")
cat("🌱 Seed used:", MANUSCRIPT_PIPELINE$reproducibility$set_seed, "\n")
cat("💻 R version:", R.version.string, "\n")
cat("📋 Complete session information saved for replication\n")

cat("\n" %.% 70, "\n")
cat("🎊 HILDA PERSONALITY VALIDATION ANALYSIS COMPLETE! 🎊\n")
cat("" %.% 70, "\n\n")

# Clean up helper function
`%.%` <- function(x, n) paste0(rep(x, n), collapse = "")
