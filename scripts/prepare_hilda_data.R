# =============================================================================
# HILDA DATA PREPARATION - STAGE 1 OF MANUSCRIPT PIPELINE
# =============================================================================
# 
# This script prepares HILDA data for the personality validation study.
# Compatible with manuscript_replication_pipeline.R
# 
# STAGE 1: Data Preparation
# - Imports HILDA Combined .dta files across waves  
# - Creates standardized variable names
# - Combines waves into longitudinal dataset
# - Saves prepared dataset for subsequent analysis stages
# 
# Addresses Method section: Data source preparation
# =============================================================================

# Stage identification for pipeline integration
CURRENT_STAGE <- list(
  number = 1,
  name = "Data Preparation", 
  description = "HILDA Dataset Processing and Variable Creation",
  required_input = "HILDA Combined .dta files",
  output_files = "hilda-dataset.qs",
  manuscript_section = "Method - Data Source"
)

# Clear environment
# rm(list = ls(all = TRUE))
# graphics.off()

cat("=== STAGE", CURRENT_STAGE$number, ":", CURRENT_STAGE$name, "===\n")
cat(CURRENT_STAGE$description, "\n")
cat("Preparing HILDA data for personality validation study...\n\n")

# =============================================================================
# LOAD PACKAGES
# =============================================================================

cat("Loading required packages...\n")

# Required packages for data preparation
required_packages <- c("tidyverse", "haven", "qs", "data.table", "fs")

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
dataDir <- file.path(root, "data")
outputDir <- file.path(root, "output")

# Create directories if they don't exist
for(dir_path in c(dataDir, outputDir)) {
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

cat("Project root:", root, "\n")
cat("Data directory:", dataDir, "\n")
cat("Output directory:", outputDir, "\n\n")

# =============================================================================
# HILDA DATA PATH CONFIGURATION
# =============================================================================

cat("=== HILDA DATA SOURCE CONFIGURATION ===\n")

# MODIFY THIS PATH TO YOUR HILDA DATA LOCATION
hilda_data_path <- file.path(dataDir, "raw")

cat("HILDA data path:", hilda_data_path, "\n")

# Enhanced path validation with helpful error messages
if(!dir.exists(hilda_data_path)) {
  cat("\n❌ ERROR: HILDA data path not found!\n")
  cat("Please update the 'hilda_data_path' variable above with your actual HILDA data location.\n")
  cat("The path should contain Combined*.dta files from HILDA 2023.\n\n")
  cat("Expected file structure:\n")
  cat("  HILDA 2023/\n")
  cat("    ├── Combined_a230c.dta  (Wave 1)\n")
  cat("    ├── Combined_e230c.dta  (Wave 5)\n") 
  cat("    ├── Combined_i230c.dta  (Wave 9)\n")
  cat("    ├── Combined_m230c.dta  (Wave 13)\n")
  cat("    ├── Combined_q230c.dta  (Wave 17)\n")
  cat("    └── Combined_u230c.dta  (Wave 21)\n\n")
  stop("❌ Data path not found: ", hilda_data_path)
}

cat("✓ HILDA data directory verified\n\n")

# =============================================================================
# IMPORT HILDA DATA FILES
# =============================================================================

cat("=== IMPORTING HILDA DATA FILES ===\n")

# Find HILDA data files with enhanced pattern matching
dta_files <- fs::dir_ls(path = hilda_data_path, 
                       type = "file", 
                       glob = "*Combined*.dta", 
                       recurse = TRUE)

# Validate data file availability
if(length(dta_files) == 0) {
  cat("❌ ERROR: No HILDA Combined*.dta files found!\n")
  cat("Expected files like Combined_a230c.dta, Combined_e230c.dta, etc.\n")
  cat("In directory:", hilda_data_path, "\n\n")
  cat("Available files:\n")
  available_files <- list.files(hilda_data_path, pattern = "\\.dta$")
  if(length(available_files) > 0) {
    for(file in head(available_files, 10)) {
      cat("  -", file, "\n")
    }
    if(length(available_files) > 10) {
      cat("  ... and", length(available_files) - 10, "more files\n")
    }
  } else {
    cat("  No .dta files found\n")
  }
  stop("❌ No HILDA data files found")
}

# Display found files with wave identification
cat("Found", length(dta_files), "HILDA data files:\n")
for(i in seq_along(dta_files)) {
  file_name <- basename(dta_files[i])
  wave_letter <- str_extract(file_name, "(?<=Combined_)[a-z]")
  wave_info <- if(!is.na(wave_letter)) paste0("(Wave ", wave_letter, ")") else ""
  cat("  ", i, ".", file_name, wave_info, "\n")
}
cat("\n")

# =============================================================================
# READ AND PROCESS DATA FILES
# =============================================================================

cat("Reading and processing data files...\n")

# Initialize data storage
data <- list()
files_processed <- 0
total_observations <- 0

# Read all data files with enhanced error handling and progress tracking
for(i in seq_along(dta_files)) {
  file_path <- dta_files[i]
  file_name <- basename(file_path)
  
  cat("Reading file", i, "of", length(dta_files), ":", file_name, "...")
  
  tryCatch({
    # Read data file
    file_data <- haven::read_dta(file_path)
    
    # Store in list with progress reporting
    data[[i]] <- file_data
    files_processed <- files_processed + 1
    file_rows <- nrow(file_data)
    total_observations <- total_observations + file_rows
    
    cat(" ✓", format(file_rows, big.mark = ","), "observations\n")
    
  }, error = function(e) {
    cat(" ❌ FAILED\n")
    cat("Error:", e$message, "\n")
    stop("❌ Failed to read file: ", file_path)
  })
}

# Name the list elements for reference
names(data) <- basename(dta_files)

cat("✓ All files read successfully\n")
cat("Files processed:", files_processed, "\n")
cat("Total observations:", format(total_observations, big.mark = ","), "\n\n")

# =============================================================================
# PROCESS VARIABLE NAMES (REMOVE WAVE PREFIXES)
# =============================================================================

cat("=== PROCESSING VARIABLE NAMES ===\n")
cat("Removing wave prefixes from variable names...\n")

variables_processed <- 0

for(i in 1:length(data)) {
  original_names <- names(data[[i]])
  wave_letter <- letters[i]  # Adjust if needed based on your file naming
  
  # Remove wave prefix (e.g., "a" from "aage" -> "age")
  names(data[[i]]) <- sub(paste0("^", wave_letter), "", names(data[[i]]))
  
  variables_processed <- variables_processed + length(original_names)
  cat("Wave", i, "(", wave_letter, "):", length(original_names), "variables processed\n")
}

cat("✓ Variable name processing completed\n")
cat("Total variables processed:", format(variables_processed, big.mark = ","), "\n\n")

# =============================================================================
# COMBINE DATA ACROSS WAVES
# =============================================================================

cat("=== COMBINING DATA ACROSS WAVES ===\n")
cat("Creating longitudinal dataset...\n")

# Combine datasets with enhanced tracking
tryCatch({
  # Use data.table for efficient binding
  data <- data.table::rbindlist(data, fill = TRUE, idcol = "source")
  
  cat("✓ Combined dataset created\n")
  cat("Final dimensions:", format(nrow(data), big.mark = ","), "observations x", 
      format(ncol(data), big.mark = ","), "variables\n\n")
  
}, error = function(e) {
  cat("❌ Failed to combine datasets\n")
  stop("Error in data combination: ", e$message)
})

# =============================================================================
# CREATE STUDY VARIABLES
# =============================================================================

cat("=== CREATING STUDY VARIABLES ===\n")
cat("Selecting and renaming variables for analysis...\n")

# Apply variable selection and renaming
df <- data %>%
  mutate(
    # Core identifiers
    individual = xwaveid,
    age = hgage,
    gender = hgsex, 
    birth_country = anbcob,
    education = edhigh1,
    employment_status = esdtl,
    
    # Big Five composite scales (if available)
    agreeableness = pnagree,
    conscientiousness = pnconsc, 
    emotional_stability = pnemote,
    extroversion = pnextrv,
    openness_to_experience = pnopene,
    
    # Agreeableness items
    sympathetic = pnsymp,
    cooperative = pncoop,
    warm = pnwarm,
    kind = pnkind,
    
    # Conscientiousness items
    orderly = pnorder,
    systematic = pnsyst,
    inefficient = pnineff,
    sloppy = pnsoppy,
    disorganised = pndorg,
    efficient = pneffic,
    
    # Neuroticism items (reverse of emotional stability)
    envious = pnenvy,
    moody = pnmoody,
    touchy = pntouch,
    jealous = pnjeal,
    temperamental = pntemp,
    fretful = pnfret,
    
    # Extraversion items
    talkative = pntalk,
    bashful = pnbful,
    quiet = pnquiet,
    shy = pnshy,
    lively = pnlivly,
    extroverted = pnextro,
    
    # Openness to Experience items
    deep = pndeep,
    philosophical = pnphil,
    creative = pncreat,
    intellectual = pnintel,
    complex = pncompx,
    imaginative = pnimag,
    
    # Validation variables
    life_satisfaction = losat,
    relationship_satisfaction1 = lsrelsp,
    relationship_satisfaction2 = lsrelsc,
    relationship_satisfaction3 = lsrelpc,
    relationship_satisfaction4 = lsrelst,
    relationship_satisfaction5 = lsrelch,
    relationship_satisfaction6 = lsrelrp,
    relationship_satisfaction7 = lsrelrs,
    relationship_satisfaction8 = lsrelfs,
    
    # Health variables
    general_health = ghgh,
    social_functioning = ghsf,
    physical_functioning = ghpf,
    role_physical = ghrp,
    role_emotional = ghre,
    bodily_pain = ghbp,
    vitality = ghvt,
    mental_health = ghmh,
    
    # Behavioral variables
    smoking = lssmkf,
    alcohol1 = lsdrka,
    alcohol2 = lsdrkf,
    income = tifditp,
    occupational_status1 = jbmo6s,
    occupational_status2 = pjoto6s,
    occupational_status3 = ujljo6s
  ) %>%
  select(source, individual:occupational_status3)  # Select only the variables we need

cat("✓ Study variables created\n")
cat("Variables selected:", ncol(df) - 1, "(excluding source identifier)\n")
cat("Final dataset:", format(nrow(df), big.mark = ","), "observations\n\n")

# =============================================================================
# ADD WAVE INFORMATION
# =============================================================================

cat("=== ADDING WAVE INFORMATION ===\n")

# Extract wave information from source filename
df <- df %>%
  mutate(
    # Extract wave letter from source filename 
    wave_letter = str_extract(source, "(?<=Combined_)[a-z]"),
    
    # Convert to wave number based on HILDA structure
    survey_wave = case_when(
      wave_letter == "e" ~ 5,   # 2005
      wave_letter == "i" ~ 9,   # 2009  
      wave_letter == "m" ~ 13,  # 2013
      wave_letter == "q" ~ 17,  # 2017
      wave_letter == "u" ~ 21,  # 2021
      TRUE ~ match(wave_letter, letters)  # fallback for other waves
    ),
    
    # Add survey year
    survey_year = 2000 + survey_wave
  ) %>%
  select(-wave_letter)  # Remove temporary variable

# Summary of waves for verification
wave_summary <- df %>%
  group_by(survey_wave, survey_year) %>%
  summarise(n_obs = n(), .groups = 'drop') %>%
  arrange(survey_wave)

cat("✓ Wave information added\n")
cat("Waves available:\n")
print(wave_summary)
cat("\n")

# =============================================================================
# SAVE PREPARED DATASET - PIPELINE COMPATIBLE
# =============================================================================

cat("=== SAVING PREPARED DATASET ===\n")

# Determine output file path (pipeline compatible)
output_file <- file.path(data_path, "hilda-dataset.qs")

tryCatch({
  # Save the prepared dataset
  qs::qsave(df, output_file)
  cat("✓ Dataset saved to:", output_file, "\n")
  
  # Verify file was saved successfully
  if(file.exists(output_file)) {
    file_size_mb <- round(file.size(output_file) / (1024^2), 1)
    cat("✓ File verification successful\n")
    cat("File size:", file_size_mb, "MB\n")
  } else {
    stop("❌ ERROR: Failed to save dataset!")
  }
  
}, error = function(e) {
  cat("❌ Failed to save dataset\n")
  stop("Error saving file: ", e$message)
})

# =============================================================================
# STAGE COMPLETION SUMMARY
# =============================================================================

cat("\n=== STAGE", CURRENT_STAGE$number, "COMPLETION SUMMARY ===\n")

cat("✅ Data preparation completed successfully!\n\n")

cat("📊 PROCESSING SUMMARY:\n")
cat("• Source files processed:", length(dta_files), "\n")
cat("• Total observations:", format(nrow(df), big.mark = ","), "\n") 
cat("• Total variables:", format(ncol(df), big.mark = ","), "\n")
cat("• Survey waves:", paste(sort(unique(df$survey_wave)), collapse = ", "), "\n")
cat("• Years covered:", min(df$survey_year, na.rm = TRUE), "-", max(df$survey_year, na.rm = TRUE), "\n")
cat("• Output file:", basename(output_file), "\n\n")

cat("📁 OUTPUT LOCATION:\n")
cat("• Prepared dataset:", output_file, "\n\n")

cat("🔄 PIPELINE INTEGRATION:\n")
cat("• Stage 1 (Data Preparation): ✅ COMPLETE\n")
cat("• Ready for Stage 2 (Participants and Measures)\n")
cat("• Dataset will be automatically loaded by subsequent analysis stages\n\n")

cat("📋 NEXT STEPS:\n")
cat("1. The prepared dataset is now ready for analysis\n")
cat("2. Run Stage 2 (participants_and_measures.R) or the full pipeline\n")
cat("3. All subsequent stages will automatically load this dataset\n\n")

# Display first few rows as verification
cat("📄 DATA VERIFICATION (First 5 rows):\n")
verification_cols <- c("individual", "survey_wave", "survey_year", "age", "gender", "agreeableness", "kind")
available_cols <- intersect(verification_cols, colnames(df))

if(length(available_cols) > 0) {
  print(head(df %>% select(all_of(available_cols)), 5))
} else {
  print(head(df %>% select(1:min(7, ncol(df))), 5))
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎉 STAGE 1: DATA PREPARATION COMPLETE! 🎉\n") 
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# Helper function for string repetition (if not available)
if(!exists("%.%")) {
  `%.%` <- function(x, n) paste0(rep(x, n), collapse = "")
}
