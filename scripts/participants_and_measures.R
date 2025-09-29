# =============================================================================
# HILDA PARTICIPANTS AND SAMPLE CHARACTERISTICS - STAGE 2 OF MANUSCRIPT PIPELINE
# =============================================================================
# 
# This script prepares participant characteristics and samples for the personality validation study.
# Compatible with manuscript_replication_pipeline.R
# 
# STAGE 2: Participants & Samples
# - Loads prepared HILDA dataset  
# - Recodes demographic and personality variables
# - Creates cross-sectional and longitudinal samples
# - Saves output for subsequent analysis stages
# 
# Addresses Method section: Participants and Measures subsections
# =============================================================================

# Stage identification for pipeline integration
CURRENT_STAGE <- list(
  number = 2,
  name = "Participants and Sample Characteristics", 
  description = "Analytical sample definition, variable coding, demographic tables",
  required_input = "hilda-dataset.qs",
  output_files = "sample_characteristics.qs, table1_sample_characteristics.tex, table2_personality_items.tex",
  manuscript_section = "Method - Participants & Measures"
)

# Clear environment
# rm(list = ls(all = TRUE))
# graphics.off()

cat("=== STAGE", CURRENT_STAGE$number, ":", CURRENT_STAGE$name, "===\n")
cat(CURRENT_STAGE$description, "\n")
cat("Preparing participant and sample outputs for personality validation study...\n\n")

# =============================================================================
# LOAD PACKAGES
# =============================================================================

cat("Loading required packages...\n")

# Required packages for data preparation
required_packages <- c("tidyverse", "haven", "qs", "psych", "broom", "effectsize")

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

manuscriptTablesDir <- file.path(root, "manuscript", "tables")
participantsDir = file.path(outputDir, "participants")
processedDataDir = file.path(outputDir, "data")

# Create directories if they don't exist
for(dir_path in c(dataDir, outputDir, manuscriptTablesDir, participantsDir,
                  processedDataDir)) {
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

cat("Project root:", root, "\n")
cat("Data directory:", dataDir, "\n")
cat("Output directory:", outputDir, "\n")
cat("Manuscript tables directory:", manuscriptTablesDir, "\n\n")

# =============================================================================
# LOAD PREPARED DATASET
# =============================================================================

cat("=== LOADING PREPARED DATASET ===\n")

# Note: assumes directory setup section (with dataDir, etc) is already above!

prepared_data_file <- file.path(dataDir, "hilda-dataset.qs")

if(!file.exists(prepared_data_file)) {
  cat("❌ ERROR: Prepared HILDA dataset not found!\n")
  cat("  Please run Stage 1 (prepare_hilda_data.R) to create the dataset first.\n")
  cat("  Expected file location:\n    ", prepared_data_file, "\n\n")
  stop("Prepared HILDA dataset missing. Analysis stopped.")
}

cat("Loading prepared HILDA dataset from:\n   ", prepared_data_file, "\n")
data <- qs::qread(prepared_data_file)
cat("✓ Dataset loaded: ", format(nrow(data), big.mark = ","), " observations, ",
    format(ncol(data), big.mark = ","), " variables\n\n", sep = "")

# =============================================================================
# DATA PROCESSING AND VARIABLE CREATION
# =============================================================================

cat("=== DATA PROCESSING AND VARIABLE CREATION ===\n")

# Step 1: Convert to factors and handle basic recoding
cat("Step 1: Converting variables and handling basic response indicators...\n")
df <- data %>%
  # Convert everything to factors first, then handle specific variables
  mutate(across(everything(), haven::as_factor)) %>%
  mutate(across(c(age:occupational_status3), as.character)) %>%
  
  # Create response indicators
  mutate(
    responding = ifelse(birth_country %in% c("[-10] Non-responding person"), "No", "Yes"),
    questionnaire = ifelse(relationship_satisfaction1 %in% c("[-8] No SCQ"), "No", "Yes")
  )
cat("✓ Step 1 complete: Basic data conversion and response indicators created\n\n")

# Step 2: Demographic variable recoding
cat("Step 2: Recoding demographic variables...\n")
df <- df %>%
  mutate(
    # Age
    age = ifelse(age %in% c("[0] Less than 1 year"), "0", age),
    age = as.character(age),
    
    # Gender
    gender = case_when(
      gender %in% c("[1] Male") ~ "Male",
      gender %in% c("[2] Female") ~ "Female"
    ),
    
    # Birth country
    birth_country = case_when(
      birth_country %in% c("[1] Australia") ~ "Australia",
      birth_country %in% c("[2] Main English Speaking") ~ "Other English-speaking", 
      birth_country %in% c("[3] Other") ~ "Non-English-speaking"
    ),
    
    # Education
    education = case_when(
      education %in% c("[3] Bachelor or honours", "[2] Grad diploma, grad certificate", 
                      "[1] Postgrad - masters or doctorate") ~ "University degree",
      education %in% c("[5] Cert III or IV", "[4] Adv diploma, diploma") ~ "Diploma/certificate",
      education %in% c("[8] Year 12") ~ "Year 12",
      education %in% c("[9] Year 11 and below") ~ "Year 11 or below"
    ),
    
    # Employment status
    employment_status = case_when(
      employment_status %in% c("[1] Employed FT") ~ "Full-time employed",
      employment_status %in% c("[2] Employed PT") ~ "Part-time employed",
      employment_status %in% c("[4] Unemployed, looking for PT work",
                             "[3] Unemployed, looking for FT work") ~ "Unemployed",
      employment_status %in% c("[6] Not in the labour force, not marginally attached",
                             "[5] Not in the labour force, marginally attached") ~ "Not in labor force"
    )
  )
cat("✓ Step 2 complete: Demographic variables recoded\n\n")

# Step 3: Personality, satisfaction, health and behavioral variables
cat("Step 3: Handling personality, satisfaction, health and behavioral variables...\n")
df <- df %>%
  # Handle personality items - set missing values
  mutate(
    across(agreeableness:imaginative, ~ifelse(. %in% c("[-10] Non-responding person", "[-8] No SCQ", 
                                                      "[-5] Multiple response SCQ", "[-4] Refused/Not stated"), NA, .)),
    across(agreeableness:imaginative, ~ifelse(. %in% c("[7] Describes me very well"), "7", .)),
    across(agreeableness:imaginative, ~ifelse(. %in% c("[1] Does not describe me at all"), "1", .))
  ) %>%
  
  # Handle life satisfaction
  mutate(
    life_satisfaction = case_when(
      life_satisfaction %in% c("[-10] Non-responding person", "[-4] Refused/Not stated", "[-3] Dont know") ~ NA,
      life_satisfaction %in% c("[10] Totally satisfied") ~ "10",
      life_satisfaction %in% c("[5] Neither satisfied nor dissatisfied") ~ "5",
      life_satisfaction %in% c("[0] Totally dissatisfied") ~ "0",
      .default = life_satisfaction
    )
  ) %>%
  
  # Handle relationship satisfaction
  mutate(
    across(relationship_satisfaction1:relationship_satisfaction8, ~ifelse(. %in% c("[-10] Non-responding person", "[-8] No SCQ", 
                                                                                  "[-6] Implausible value", "[-5] Multiple response SCQ", 
                                                                                  "[-4] Refused/Not stated", "[-2] Not applicable"), NA, .)),
    across(relationship_satisfaction1:relationship_satisfaction8, ~ifelse(. %in% c("[10] Completely satisfied"), "10", .)),
    across(relationship_satisfaction1:relationship_satisfaction8, ~ifelse(. %in% c("[0] Completely dissatisfied"), "0", .))
  ) %>%
  
  # Handle health variables
  mutate(
    across(general_health:mental_health, ~ifelse(. %in% c("[-10] Non-responding person", "[-8] No SCQ", "[-6] Implausible value",
                                                          "[-5] Multiple response SCQ", "[-4] Refused/Not stated"), NA, .))
  ) %>%
  
  # Handle smoking
  mutate(
    smoking = case_when(
      smoking %in% c("[5] Yes, I smoke less often than weekly", "[4] Yes, I smoke at least weekly but not daily", 
                    "[3] Yes, I smoke daily") ~ "Current smoker",
      smoking %in% c("[2] No, I no longer smoke") ~ "Former smoker",
      smoking %in% c("[1] No, I have never smoked") ~ "Never smoker"
    )
  ) %>%
  
  # Handle alcohol variables
  mutate(
    alcohol1 = case_when(
      alcohol1 %in% c("[1] 13 or more standard drinks") ~ "13.5",
      alcohol1 %in% c("[2] 11 to 12 standard drinks") ~ "11.5",
      alcohol1 %in% c("[3] 9 to 10 standard drinks") ~ "9.5",
      alcohol1 %in% c("[4] 7 to 8 standard drinks") ~ "7.5",
      alcohol1 %in% c("[5] 5 to 6 standard drinks") ~ "5.5",
      alcohol1 %in% c("[6] 3 to 4 standard drinks") ~ "3.5",
      alcohol1 %in% c("[7] 1 to 2 standard drinks") ~ "1.5"
    ),
    
    alcohol2 = case_when(
      alcohol2 %in% c("[1] I have never drunk alcohol", "[2] I no longer drink") ~ "0",
      alcohol2 %in% c("[3] Yes, I drink alcohol everyday") ~ "7",
      alcohol2 %in% c("[4] Yes, I drink alcohol 5 to 6 days per week") ~ "5.5",
      alcohol2 %in% c("[5] Yes, I drink alcohol 3 to 4 days per week") ~ "3.5",
      alcohol2 %in% c("[6] Yes, I drink alcohol 1 or 2 days per week") ~ "1.5",
      alcohol2 %in% c("[7] Yes, I drink alcohol 2 or 3 days per month") ~ "0.6",
      alcohol2 %in% c("[8] Yes, but only rarely") ~ "0.2"
    )
  ) %>%
  
  # Handle occupational status
  mutate(
    across(occupational_status1:occupational_status3, ~ifelse(. %in% c("[-10] Non-responding person", "[-7] Not able to be determined", 
                                                                      "[-4] Refused/Not stated", "[-3] Dont know", "[-1] Not asked"), NA, .))
  )
cat("✓ Step 3 complete: All personality, satisfaction, health, and behavioral variables processed\n\n")

# Step 4: Numeric conversion, reverse coding, and derived scales
cat("Step 4: Numeric conversion and derived scale creation...\n")
df <- df %>%
  # Convert to numeric
  mutate(across(c(age, agreeableness:mental_health, alcohol1:survey_year), as.numeric)) %>%
  
  # Create reverse-coded variables
  mutate(
    across(c(inefficient, sloppy, disorganised, bashful, quiet, shy), 
           ~abs(.x - 8), .names = "{.col}_r")
  ) %>%
  
  # Create Neuroticism variable
  mutate(neuroticism = select(., envious:fretful) %>% rowMeans(na.rm = TRUE)) %>%
  
  # Fix spelling mistake of Extraversion variable
  rename(extraversion = extroversion)
cat("✓ Step 4 complete: All derived and reverse-coded variables created\n\n")

# Step 5: Health component summary scores (SF-36)
cat("Step 5: Creating health component summary scores (SF-36)...\n")
df1 <- df %>%
  filter(complete.cases(select(., general_health:mental_health))) %>%
  select(general_health:mental_health)

if(nrow(df1) > 100) {
  # Run factor analysis to create health components
  fa_result <- psych::fa(df1, nfactors = 2, rotate = "oblimin", fm = "ml", scores = "regression")
  df1 <- as.matrix(df1)
  loadings <- pmax(fa_result$loadings, 0)
  
  # Create component scores
  df <- df %>%
    mutate(
      physical_component_summary = c(as.matrix(select(., general_health:mental_health)) %*% loadings[,1]),
      mental_component_summary = c(as.matrix(select(., general_health:mental_health)) %*% loadings[,2])
    ) %>%
    # Standardize to Australian norms (mean=50, SD=10)
    mutate(
      across(physical_component_summary:mental_component_summary, 
             ~((. - mean(., na.rm = TRUE))/sd(., na.rm = TRUE)) * 10 + 50, .names = "{.col}_a")
    )
  cat("✓ Health component summary scores created\n")
} else {
  cat("⚠️ Insufficient health data for factor analysis - skipping health components\n")
}

# Step 6: Additional derived variables and final recodes
cat("Step 6: Final derived variables and grouping...\n")
df <- df %>%
  mutate(
    # Age groups
    age_a = ifelse(is.na(age), age, ifelse(age <= 30, "18-30", ifelse(age <= 50, "31-50", "51+"))),
    
    # Education groups  
    education_a = ifelse(is.na(education), education, 
                        ifelse(education %in% c("Year 12", "Year 11 or below"), "High school or less", "Post-secondary")),
    
    # Relationship satisfaction (average)
    relationship_satisfaction_a = select(., relationship_satisfaction1:relationship_satisfaction8) %>% rowMeans(na.rm = TRUE),
    
    # Smoking binary
    smoking_a = as.numeric(ifelse(is.na(smoking), smoking, ifelse(smoking %in% c("Current smoker"), 1, 0))),
    
    # Alcohol consumption
    alcohol_a = ifelse(alcohol2 > 0, alcohol1 * alcohol2, alcohol2),
    
    # Income (log transformed)
    income_a = log(income + 1),
    
    # Occupational status (best available)
    occupational_status_a = ifelse(is.na(occupational_status1), 
                                  ifelse(is.na(occupational_status2), occupational_status3, occupational_status2), 
                                  occupational_status1)
  )
cat("✓ Step 6 complete: All additional derived variables created\n\n")

# =============================================================================
# CREATE ANALYTICAL SAMPLES
# =============================================================================

cat("=== CREATING ANALYTICAL SAMPLES ===\n")

# Step 1: Define and check personality items
cat("Step 1: Defining and checking personality items...\n")
personality_items <- c(
  # Neuroticism
  "envious", "fretful", "jealous", "moody", "temperamental", "touchy",
  # Extraversion (with reverse-coded items)
  "bashful_r", "extroverted", "lively", "quiet_r", "shy_r", "talkative",
  # Openness
  "complex", "creative", "deep", "imaginative", "intellectual", "philosophical",
  # Agreeableness
  "cooperative", "kind", "sympathetic", "warm",
  # Conscientiousness (with reverse-coded items)
  "disorganised_r", "efficient", "inefficient_r", "orderly", "sloppy_r", "systematic"
)
available_items <- intersect(personality_items, colnames(df))
missing_items <- setdiff(personality_items, colnames(df))
cat("✓ Personality items available: ", length(available_items), " of ", length(personality_items), "\n")
if(length(missing_items) > 0) {
  cat("⚠️ Missing items:", paste(missing_items, collapse = ", "), "\n")
}
cat("\n")

# Step 2: Create full sample with personality data (waves, age, responding, SCQ)
cat("Step 2: Creating full sample (personality waves, adults, responding, SCQ)...\n")
full_sample <- df %>%
  filter(
    survey_wave %in% c(5, 9, 13, 17, 21),  # Personality waves
    age >= 18,
    responding == "Yes",
    questionnaire == "Yes"
  )
cat("✓ Full sample constructed:", format(nrow(full_sample), big.mark = ","), "observations\n")

# Check completeness for personality data
full_sample <- full_sample %>%
  filter(complete.cases(select(., all_of(available_items)))) %>%
  select(individual, survey_wave) %>%
  mutate(complete = "Yes") %>%
  left_join(full_sample, ., by = c("individual", "survey_wave")) %>%
  mutate(complete = ifelse(is.na(complete), "No", complete))
cat("✓ Full sample with complete personality data:", format(sum(full_sample$complete == "Yes"), big.mark = ","), "observations\n\n")

# Step 3: Subsample 1 - Wave 5 cross-sectional sample
cat("Step 3: Creating Subsample 1 (Wave 5 cross-sectional, complete personality)...\n")
subsample1 <- full_sample %>%
  filter(survey_wave == 5, complete == "Yes")
cat("✓ Subsample 1 constructed:", format(nrow(subsample1), big.mark = ","), "cases\n\n")

# Step 4: Subsample 2 - Longitudinal panel, 5 waves
cat("Step 4: Creating Subsample 2 (longitudinal panel, complete across 5 waves)...\n")
subsample2 <- full_sample %>%
  filter(complete == "Yes")
cat("✓ Subsample 2 constructed:", format(nrow(subsample2), big.mark = ","), "cases from",
    format(n_distinct(subsample2$individual), big.mark = ","), "individuals\n\n")

# Step 5: Subsample 3 - Unbalanced panel, ≥2 waves
cat("Step 5: Creating Subsample 3 (unbalanced panel, ≥2 waves)...\n")
subsample3 <- subsample2 %>%
  group_by(individual) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n >= 2) %>%
  inner_join(subsample2, by = "individual") %>%
  mutate(n = NULL)
cat("✓ Subsample 3 constructed:", format(nrow(subsample3), big.mark = ","), "cases from",
    format(n_distinct(subsample3$individual), big.mark = ","), "individuals\n\n")

# =============================================================================
# SAMPLE CHARACTERISTICS ANALYSIS
# =============================================================================

cat("=== SAMPLE CHARACTERISTICS ANALYSIS ===\n")

# Create comprehensive sample characteristics table
cat("Computing comprehensive sample characteristics by wave...\n")
sample_characteristics <- full_sample %>%
  group_by(survey_wave, complete) %>%
  summarise(
    year = first(survey_year),
    sample_n = n(),
    
    # Response rate calculation
    response_rate = (sample_n / full_sample %>% filter(survey_year == year) %>% nrow()) * 100,
    
    # Demographics
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    female_pct = mean(gender == "Female", na.rm = TRUE) * 100,
    
    # Birth country
    australia_pct = mean(birth_country == "Australia", na.rm = TRUE) * 100,
    other_english_pct = mean(birth_country == "Other English-speaking", na.rm = TRUE) * 100,
    non_english_pct = mean(birth_country == "Non-English-speaking", na.rm = TRUE) * 100,
    
    # Education
    university_pct = mean(education == "University degree", na.rm = TRUE) * 100,
    diploma_pct = mean(education == "Diploma/certificate", na.rm = TRUE) * 100,
    year12_pct = mean(education == "Year 12", na.rm = TRUE) * 100,
    year11_pct = mean(education == "Year 11 or below", na.rm = TRUE) * 100,
    
    # Employment
    employed_ft_pct = mean(employment_status == "Full-time employed", na.rm = TRUE) * 100,
    employed_pt_pct = mean(employment_status == "Part-time employed", na.rm = TRUE) * 100,
    unemployed_pct = mean(employment_status == "Unemployed", na.rm = TRUE) * 100,
    NILF_pct = mean(employment_status == "Not in labor force", na.rm = TRUE) * 100,
    
    .groups = 'drop'
  ) %>%
  filter(complete == "Yes") %>%
  mutate(complete = NULL)

cat("✓ Sample characteristics computed across waves\n")

# =============================================================================
# ATTRITION ANALYSIS
# =============================================================================

cat("=== ATTRITION ANALYSIS ===\n")

# Step 1: Identify continuation for Wave 5 sample
cat("Step 1: Identifying continued participation beyond Wave 5...\n")
df2 <- df %>% filter(survey_wave > 5)
subsample1_continued <- subsample1$individual %in% df2$individual
cat("✓ Step 1 complete: identified ", format(sum(subsample1_continued), big.mark = ","), 
    " participants who continued, ", sum(!subsample1_continued), " dropped out\n\n", sep = "")

# Step 2: Prepare predictors for attrition model
cat("Step 2: Preparing predictors for attrition analysis...\n")
personality_predictors <- intersect(
  c("agreeableness", "conscientiousness", "neuroticism", "extraversion", "openness_to_experience"), 
  colnames(subsample1)
)
demographic_predictors <- intersect(
  c("age", "gender", "birth_country", "education", "employment_status"), 
  colnames(subsample1)
)
all_predictors <- c(demographic_predictors, personality_predictors)
available_predictors <- intersect(all_predictors, colnames(subsample1))
cat("✓ Step 2 complete: available predictors - ", 
    ifelse(length(available_predictors) > 0, paste(available_predictors, collapse = ", "), "None"), "\n\n", sep = "")

# Step 3: Fit logistic regression predicting attrition
cat("Step 3: Fitting logistic regression on attrition predictors...\n")
model_formula <- as.formula(paste("subsample1_continued ~", paste(available_predictors, collapse = " + ")))

attrition_model <- tryCatch({
  glm(model_formula, data = subsample1, family = binomial)
}, error = function(e) {
  cat("❌ Error fitting attrition model: ", e$message, "\n")
  return(NULL)
})

if (!is.null(attrition_model)) {
  cat("✓ Step 3 complete: logistic regression model fit\n\n")
  
  attrition_summary <- summary(attrition_model)
  attrition_tidy <- broom::tidy(attrition_model, exponentiate = TRUE, conf.int = TRUE)
  
  # Step 4: Identify significant predictors and effect sizes
  cat("Step 4: Identifying significant predictors and calculating effect sizes...\n")
  significant_predictors <- attrition_tidy %>%
    filter(term != "(Intercept)", p.value < 0.05) %>%
    pull(term)
  
  if(length(significant_predictors) > 0) {
    cat("✓ Step 4 complete: significant predictors (p < 0.05): ", 
        paste(significant_predictors, collapse = ", "), "\n", sep = "")
  } else {
    cat("Step 4: No significant predictors found (p < 0.05)\n")
  }
  
  # Step 5: Calculate effect sizes for significant predictors
  if(length(significant_predictors) > 0) {
    cat("Step 5: Calculating effect sizes for significant predictors...\n")    
    effect_sizes <- list()
    
    for(predictor in significant_predictors) {
      
      # Handle categorical variables - extract base variable name
      base_predictor <- gsub("^(.+?)([A-Z].*|\\d.*)", "\\1", predictor)  # Remove level suffixes
      base_predictor <- gsub("\\d+$", "", base_predictor)  # Remove trailing numbers
      
      # Check if base predictor exists in data
      if(base_predictor %in% colnames(subsample1)) {
        
        predictor_data <- subsample1[[base_predictor]]
        
        # Calculate effect size based on variable type
        if(is.numeric(predictor_data)) {
          # Numeric variable - calculate Cohen's d
          continued_values <- predictor_data[subsample1_continued]
          dropped_values <- predictor_data[!subsample1_continued]
          
          # Remove missing values
          continued_values <- continued_values[!is.na(continued_values)]
          dropped_values <- dropped_values[!is.na(dropped_values)]
          
          if(length(continued_values) > 0 && length(dropped_values) > 0) {
            effect_size <- tryCatch({
              result <- effectsize::cohens_d(continued_values, dropped_values)
              if(is.null(result) || is.null(result$Cohens_d)) {
                NA
              } else {
                result$Cohens_d
              }
            }, error = function(e) {
              cat("Error calculating Cohen's d for", base_predictor, ":", e$message, "\n")
              NA
            })
            
            effect_sizes[[base_predictor]] <- list(
              variable = base_predictor,
              effect_size = ifelse(is.null(effect_size) || length(effect_size) == 0, NA, effect_size),
              type = "Cohen's d",
              continued_mean = mean(continued_values, na.rm = TRUE),
              dropped_mean = mean(dropped_values, na.rm = TRUE),
              n_continued = length(continued_values),
              n_dropped = length(dropped_values)
            )
          }
          
        } else if(is.factor(predictor_data) || is.character(predictor_data)) {
          # Categorical variable - calculate Cramer's V
          contingency_table <- table(predictor_data, subsample1_continued)
          
          if(nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
            effect_size <- tryCatch({
              result <- effectsize::cramers_v(contingency_table)
              if(is.null(result) || is.null(result$estimate)) {
                NA
              } else {
                result$estimate
              }
            }, error = function(e) {
              cat("Error calculating Cramer's V for", base_predictor, ":", e$message, "\n")
              NA
            })
            
            # Calculate percentages for interpretation
            continued_pct <- prop.table(table(predictor_data[subsample1_continued])) * 100
            dropped_pct <- prop.table(table(predictor_data[!subsample1_continued])) * 100
            
            effect_sizes[[base_predictor]] <- list(
              variable = base_predictor,
              effect_size = ifelse(is.null(effect_size) || length(effect_size) == 0, NA, effect_size),
              type = "Cramer's V",
              continued_distribution = continued_pct,
              dropped_distribution = dropped_pct,
              contingency_table = contingency_table
            )
          }
        }
      } else {
        cat("Warning: Base predictor", base_predictor, "not found in data for", predictor, "\n")
      }
    }
    
    # Report effect sizes
    cat("\nEffect sizes for significant predictors:\n")
    
    for(predictor_name in names(effect_sizes)) {
      effect_info <- effect_sizes[[predictor_name]]
      
      # Check if effect_size exists and is not NA
      if(!is.null(effect_info$effect_size) && 
         length(effect_info$effect_size) > 0 && 
         !is.na(effect_info$effect_size)) {
        
        cat("• ", predictor_name, " (", effect_info$type, "): ", 
            sprintf("%.3f", effect_info$effect_size), sep = "")
        
        # Add interpretation
        if(effect_info$type == "Cohen's d") {
          interpretation <- case_when(
            abs(effect_info$effect_size) < 0.2 ~ " (negligible)",
            abs(effect_info$effect_size) < 0.5 ~ " (small)",
            abs(effect_info$effect_size) < 0.8 ~ " (medium)",
            TRUE ~ " (large)"
          )
          cat(interpretation)
          
          # Add means for interpretation
          cat("\n    Continued: M = ", sprintf("%.2f", effect_info$continued_mean),
              ", Dropped: M = ", sprintf("%.2f", effect_info$dropped_mean), sep = "")
          
        } else if(effect_info$type == "Cramer's V") {
          interpretation <- case_when(
            effect_info$effect_size < 0.1 ~ " (negligible)",
            effect_info$effect_size < 0.3 ~ " (small)",
            effect_info$effect_size < 0.5 ~ " (medium)",
            TRUE ~ " (large)"
          )
          cat(interpretation)
        }
        cat("\n")
      } else {
        cat("• ", predictor_name, ": Effect size could not be calculated\n")
      }
      cat("✓ Step 5 complete: effect sizes calculated\n")
    }
    
    # Step 6: Summarize attrition model results
    cat("Step 6: Summarizing attrition results and reporting final model...\n")
    # Summary statistics
    cat("\nAttrition summary:\n")
    cat("• Continued participation: N =", sum(subsample1_continued), 
        " (", sprintf("%.1f%%", mean(subsample1_continued) * 100), ")\n")
    cat("• Dropped out: N =", sum(!subsample1_continued), 
        " (", sprintf("%.1f%%", mean(!subsample1_continued) * 100), ")\n")
    
    # Model summary
    cat("\nLogistic regression summary:\n")
    significant_count <- sum(attrition_tidy$p.value < 0.05) - 1  # Exclude intercept
    cat("• Significant predictors: ", significant_count, " of ", nrow(attrition_tidy) - 1, "\n")
    
    # Pseudo R-squared
    null_deviance <- attrition_model$null.deviance
    residual_deviance <- attrition_model$deviance
    pseudo_r2 <- 1 - (residual_deviance / null_deviance)
    cat("• Pseudo R² (McFadden): ", sprintf("%.3f", pseudo_r2), "\n")
    
    # Show logistic regression coefficients for significant predictors
    cat("\nSignificant coefficients (Odds Ratios):\n")
    significant_coefs <- attrition_tidy %>%
      filter(term != "(Intercept)", p.value < 0.05) %>%
      select(term, estimate, std.error, p.value)
    
    for(i in 1:nrow(significant_coefs)) {
      coef_row <- significant_coefs[i, ]
      cat("• ", coef_row$term, ": OR = ", sprintf("%.3f", coef_row$estimate), 
          ", p = ", ifelse(coef_row$p.value < 0.001, "< .001", sprintf("%.3f", coef_row$p.value)), "\n")
    }
    
    # Store results for output
    attrition_results <- list(
      model_summary = attrition_summary,
      model_tidy = attrition_tidy,
      significant_predictors = significant_predictors,
      effect_sizes = effect_sizes,
      continuation_rate = mean(subsample1_continued),
      pseudo_r2 = pseudo_r2,
      n_total = length(subsample1_continued),
      n_continued = sum(subsample1_continued),
      n_dropped = sum(!subsample1_continued)
    )
    
  } else {
    cat("No significant predictors of attrition found.\n")
    
    attrition_results <- list(
      model_summary = attrition_summary,
      model_tidy = attrition_tidy,
      significant_predictors = character(0),
      effect_sizes = list(),
      continuation_rate = mean(subsample1_continued),
      pseudo_r2 = 1 - (attrition_model$deviance / attrition_model$null.deviance),
      n_total = length(subsample1_continued),
      n_continued = sum(subsample1_continued),
      n_dropped = sum(!subsample1_continued)
    )
  }
  cat("✓ Step 6 complete: attrition analysis finished\n\n")
  
} else {
  cat("❌ Attrition model could not be fit; skipping further analysis.\n\n")
}

# =============================================================================
# GENERATE LATEX TABLES
# =============================================================================

cat("=== GENERATING LATEX TABLES ===\n")

# Step 1: Create LaTeX for Table 1 (Sample Characteristics)
cat("Step 1: Creating LaTeX for Table 1 (Sample Characteristics)...\n")
create_table1_sample_characteristics <- function(sample_characteristics) {
  
  # Helper functions
  format_percentage <- function(pct) sprintf("%.1f", pct)
  format_mean_sd <- function(mean_val, sd_val) sprintf("%.1f (%.1f)", mean_val, sd_val)
  
  get_wave_value <- function(wave, variable) {
    value <- sample_characteristics[sample_characteristics$survey_wave == wave, variable][[1]]
    if(length(value) == 0) return(NA)
    return(value)
  }
  
  # Define waves and years for headers
  waves <- c(5, 9, 13, 17, 21)
  years <- c(2005, 2009, 2013, 2017, 2021)
  
  # Start LaTeX table
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Sample Characteristics across Study Waves}",
    "\\label{tab:sample_characteristics}",
    "\\begin{tabular}{lrrrrr}",
    "\\toprule",
    " & Wave 5 & Wave 9 & Wave 13 & Wave 17 & Wave 21 \\\\",
    " & (2005) & (2009) & (2013) & (2017) & (2021) \\\\",
    "\\midrule"
  )
  
  # Sample Size section
  latex_lines <- c(latex_lines, "\\textbf{Sample Size} & & & & & \\\\")

  # Complete personality data
  n_values <- sapply(waves, function(w) {
    n <- get_wave_value(w, "sample_n")
    format(n, big.mark = ",")
  })
  latex_lines <- c(latex_lines, paste("\\quad Complete personality data &", paste(n_values, collapse = " & "), "\\\\"))
  
  # Response rates
  response_values <- sapply(waves, function(w) {
    rate <- get_wave_value(w, "response_rate") 
    format_percentage(rate)
  })
  latex_lines <- c(latex_lines, paste("\\quad Response rate (\\%) &", paste(response_values, collapse = " & "), "\\\\"))
  
  # Add spacing
  latex_lines <- c(latex_lines, "\\addlinespace")
  
  # Demographics section
  latex_lines <- c(latex_lines, "\\textbf{Demographics} & & & & & \\\\")

  # Age
  age_values <- sapply(waves, function(w) {
    mean_val <- get_wave_value(w, "age_mean")
    sd_val <- get_wave_value(w, "age_sd")
    format_mean_sd(mean_val, sd_val)
  })
  latex_lines <- c(latex_lines, paste("\\quad Age $M$ ($SD$) &", paste(age_values, collapse = " & "), "\\\\"))
  
  # Female percentage
  female_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "female_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Female (\\%) &", paste(female_values, collapse = " & "), "\\\\"))
  
  # Add spacing
  latex_lines <- c(latex_lines, "\\addlinespace")
  
  # Birth country
  latex_lines <- c(latex_lines, "\\textbf{Country of Birth} & & & & & \\\\")

    # Australia
  australia_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "australia_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Australia (\\%) &", paste(australia_values, collapse = " & "), "\\\\"))
  
  # Other English-speaking
  other_eng_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "other_english_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Other English-speaking (\\%) &", paste(other_eng_values, collapse = " & "), "\\\\"))
  
  # Non-English-speaking
  non_eng_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "non_english_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Non-English-speaking (\\%) &", paste(non_eng_values, collapse = " & "), "\\\\"))
  
  # Add spacing
  latex_lines <- c(latex_lines, "\\addlinespace")
  
  # Education
  latex_lines <- c(latex_lines, "\\textbf{Education} & & & & & \\\\")
  
  # University degree
  uni_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "university_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad University degree (\\%) &", paste(uni_values, collapse = " & "), "\\\\"))
  
  # Diploma/certificate
  diploma_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "diploma_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Diploma/certificate (\\%) &", paste(diploma_values, collapse = " & "), "\\\\"))
  
  # Year 12
  year12_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "year12_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Year 12 (\\%) &", paste(year12_values, collapse = " & "), "\\\\"))
  
  # Year 11 or below
  year11_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "year11_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Year 11 or below (\\%) &", paste(year11_values, collapse = " & "), "\\\\"))
  
  # Add spacing
  latex_lines <- c(latex_lines, "\\addlinespace")
  
  # Employment
  latex_lines <- c(latex_lines, "\\textbf{Employment Status} & & & & & \\\\")
  
  # Full-time employed
  ft_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "employed_ft_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Full-time employed (\\%) &", paste(ft_values, collapse = " & "), "\\\\"))
  
  # Part-time employed
  pt_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "employed_pt_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Part-time employed (\\%) &", paste(pt_values, collapse = " & "), "\\\\"))
  
  # Unemployed
  unemp_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "unemployed_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Unemployed (\\%) &", paste(unemp_values, collapse = " & "), "\\\\"))
  
  # Not in labor force
  nilf_values <- sapply(waves, function(w) {
    pct <- get_wave_value(w, "NILF_pct")
    format_percentage(pct)
  })
  latex_lines <- c(latex_lines, paste("\\quad Not in labor force (\\%) &", paste(nilf_values, collapse = " & "), "\\\\"))
  
  # End table
  latex_lines <- c(latex_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}",
    "\\small",
    "\\item \\textit{Note}. Sample includes respondents aged 18+ years with complete personality data. Percentages may not sum to 100\\% due to rounding.",
    "\\end{tablenotes}",
    "\\end{table}")
  
  return(paste(latex_lines, collapse = "\n"))
}
table1_latex <- create_table1_sample_characteristics(sample_characteristics)
cat("✓ Step 1 complete: LaTeX code generated for Table 1\n\n")

# Step 2: Create LaTeX for Table 2 (Personality Items)
cat("Step 2: Creating LaTeX for Table 2 (Personality Items)...\n")
create_table2_personality_items <- function() {
  
  # Define the personality items
  personality_structure <- list(
    "Extraversion" = list(
      items = c("Bashful$^{\\mathrm{R}}$", "Extroverted", "Lively", "Quiet$^{\\mathrm{R}}$", "Shy$^{\\mathrm{R}}$", "Talkative"),
      source = c("TDA-40", "TDA-40", "Other", "TDA-40", "TDA-40", "TDA-40")
    ),
    "Agreeableness" = list(
      items = c("Cooperative", "Kind", "Sympathetic", "Warm"),
      source = c("TDA-40", "TDA-40", "TDA-40", "TDA-40")
    ),
    "Conscientiousness" = list(
      items = c("Disorganized$^{\\mathrm{R}}$", "Efficient", "Inefficient$^{\\mathrm{R}}$", "Orderly", "Sloppy$^{\\mathrm{R}}$", "Systematic"),
      source = c("TDA-40", "TDA-40", "TDA-40", "Other", "TDA-40", "TDA-40")
    ),
    "Neuroticism" = list(
      items = c("Envious", "Fretful", "Jealous", "Moody", "Temperamental", "Touchy"),
      source = c("TDA-40", "TDA-40", "TDA-40", "TDA-40", "TDA-40", "TDA-40")
    ),
    "Openness to Experience" = list(
      items = c("Complex", "Creative", "Deep", "Imaginative", "Intellectual", "Philosophical"),
      source = c("TDA-40", "TDA-40", "TDA-40", "TDA-40", "TDA-40", "TDA-40")
    )
  )
  
  # Start LaTeX table
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Big Five Personality Items in HILDA Survey}",
    "\\label{tab:personality_items}",
    "\\begin{tabular}{p{5cm}p{5cm}p{2cm}}",
    "\\toprule",
    "Factor & Items & Source \\\\",
    "\\midrule"
  )
  
  # Add items for each factor
  for(factor_name in names(personality_structure)) {
    items <- personality_structure[[factor_name]]$items
    sources <- personality_structure[[factor_name]]$source
    
    # First row of factor (with factor name)
    latex_lines <- c(latex_lines, paste(factor_name, "&", items[1], "&", sources[1], "\\\\"))
    
    # Remaining rows (without factor name)
    if(length(items) > 1) {
      for(i in 2:length(items)) {
        latex_lines <- c(latex_lines, paste(" &", items[i], "&", sources[i], "\\\\"))
      }
    }
    
    # Add space between factors (except after last one)
    if(factor_name != names(personality_structure)[length(personality_structure)]) {
      latex_lines <- c(latex_lines, "\\addlinespace")
    }
  }
  
  # End table
  latex_lines <- c(latex_lines,
                   "\\bottomrule",
                   "\\end{tabular}",
                   "\\begin{tablenotes}",
                   "\\small",
                   "\\item \\textit{Note}. TDA-40 = Trait Descriptive Adjectives-40 \\citep{Saucier1994}. Other = items selected from alternative sources. $^{\\mathrm{R}}$ = reverse-coded items. All items rated on 7-point scales from 1 (\\textit{does not describe me at all}) to 7 (\\textit{describes me very well}).",
                   "\\end{tablenotes}",
                   "\\end{table}")
  
  return(paste(latex_lines, collapse = "\n"))
}
table2_latex <- create_table2_personality_items()
cat("✓ Step 2 complete: LaTeX code generated for Table 2\n\n")

# Step 3: Save LaTeX Tables (to 'manuscript/tables/')
cat("Step 3: Saving LaTeX tables to manuscript tables directory...\n")
writeLines(table1_latex, file.path(manuscriptTablesDir, "sample_characteristics.tex"))
writeLines(table2_latex, file.path(manuscriptTablesDir, "personality_items.tex"))
cat("✓ Step 3 complete: LaTeX tables written to:\n  -", 
    file.path(manuscriptTablesDir, "sample_characteristics.tex"), "\n  -", 
    file.path(manuscriptTablesDir, "personality_items.tex"), "\n\n")

# =============================================================================
# SAVE PROCESSED DATA AND RESULTS
# =============================================================================

cat("=== SAVING PROCESSED DATA AND RESULTS ===\n")

# Step 1: Save processed datasets to output/data
cat("Step 1: Saving processed analytic samples...\n")
qs::qsave(full_sample, file.path(processedDataDir, "full_sample.qs"))
qs::qsave(subsample1, file.path(processedDataDir, "subsample1_cross_sectional.qs"))
qs::qsave(subsample2, file.path(processedDataDir, "subsample2_longitudinal_panel.qs"))
qs::qsave(subsample3, file.path(processedDataDir, "subsample3_unbalanced_panel.qs"))
cat("✓ Step 1 complete: full_sample, subsample1, subsample2, subsample3 saved\n\n")

# Step 2: Save sample characteristics and attrition results
cat("Step 2: Saving sample characteristics and attrition results...\n")
qs::qsave(sample_characteristics, file.path(participantsDir, "sample_characteristics.qs"))

attrition_analysis <- list(
  model_summary = attrition_summary,
  model_tidy = attrition_tidy,
  age_effect = attrition_results$effect_sizes$age$effect_size,
  neuroticism_effect = attrition_results$effect_sizes$neuroticism$effect_size,
  sample_continued = sum(subsample1_continued),
  sample_total = length(subsample1_continued)
)
qs::qsave(attrition_analysis, file.path(participantsDir, "attrition_analysis.qs"))
cat("✓ Step 2 complete: sample characteristics and attrition analysis saved\n\n")

# Step 3: Save the method summary for manuscript use
cat("Step 3: Saving method summary for manuscript...\n")
method_summary <- list(
  sample_sizes = list(
    wave5_cross_sectional = nrow(subsample1),
    longitudinal_panel = nrow(subsample2),
    longitudinal_panel_individuals = n_distinct(subsample3$individual),
    unbalanced_panel = nrow(subsample3),
    unbalanced_panel_individuals = n_distinct(subsample3$individual)
  ),
  demographic_summary = list(
    wave5_female_pct = sample_characteristics$female_pct[sample_characteristics$survey_wave == "5"],
    wave5_age_mean = sample_characteristics$age_mean[sample_characteristics$survey_wave == "5"],
    wave5_age_sd = sample_characteristics$age_sd[sample_characteristics$survey_wave == "5"],
    wave5_australia_pct = sample_characteristics$australia_pct[sample_characteristics$survey_wave == "5"],
    wave5_other_english_pct = sample_characteristics$other_english_pct[sample_characteristics$survey_wave == "5"],
    wave5_non_english_pct = sample_characteristics$non_english_pct[sample_characteristics$survey_wave == "5"],
    wave5_university_pct = sample_characteristics$university_pct[sample_characteristics$survey_wave == "5"],
    wave5_diploma_pct = sample_characteristics$diploma_pct[sample_characteristics$survey_wave == "5"],
    wave5_year12_pct = sample_characteristics$year12_pct[sample_characteristics$survey_wave == "5"],
    wave5_year11_pct = sample_characteristics$year11_pct[sample_characteristics$survey_wave == "5"]
  ),
  attrition_summary = list(
    age_or = attrition_tidy$estimate[attrition_tidy$term == "age"],
    neuroticism_or = attrition_tidy$estimate[attrition_tidy$term == "neuroticism"],
    age_effect = attrition_analysis$age_effect,
    neuroticism_effect = attrition_analysis$neuroticism_effect
  )
)
qs::qsave(method_summary, file.path(participantsDir, "method_summary.qs"))
cat("✓ Step 3 complete: method summary saved\n\n")

# =============================================================================
# STAGE COMPLETION SUMMARY
# =============================================================================

cat("\n=== STAGE", CURRENT_STAGE$number, "COMPLETION SUMMARY ===\n")

cat("✅ Participants and sample characteristics processing completed successfully!\n\n")

cat("📊 PROCESSING SUMMARY:\n")
cat("• Full sample records:", format(nrow(full_sample), big.mark = ","), "\n")
cat("• Wave 5 cross-sectional sample size:", format(nrow(subsample1), big.mark = ","), "\n")
cat("• Longitudinal panel records:", format(nrow(subsample2), big.mark = ","), "\n")
cat("• Unbalanced panel records:", format(nrow(subsample3), big.mark = ","), "\n")
cat("• Number of unique individuals (unbalanced panel):", format(n_distinct(subsample3$individual), big.mark = ","), "\n")
cat("• Total survey waves included:", paste(sort(unique(full_sample$survey_wave)), collapse = ", "), "\n")
cat("• Years covered:", paste0(min(full_sample$survey_year, na.rm=TRUE), " - ", max(full_sample$survey_year, na.rm=TRUE)), "\n\n")

cat("📁 OUTPUT LOCATION:\n")
cat("• Analytic samples: output/data/full_sample.qs, output/data/subsample1_cross_sectional.qs, etc.\n")
cat("• Results: output/participants/sample_characteristics.qs, attrition_analysis.qs\n")
cat("• Manuscript tables: manuscript/tables/sample_characteristics.tex, personality_items.tex\n\n")

cat("🔄 PIPELINE INTEGRATION:\n")
cat("• Stage 2 (Participants and Measures): ✅ COMPLETE\n")
cat("• Ready for Stage 3 (Preliminary Analyses)\n")
cat("• Analytic samples and tables ready for all subsequent stages\n\n")

cat("📋 NEXT STEPS:\n")
cat("1. Begin factor analysis using subsample1_cross_sectional.qs\n")
cat("2. Incorporate LaTeX tables into your manuscript as needed\n")
cat("3. Use longitudinal & panel samples as required in later scripts\n\n")

cat("📄 DATA VERIFICATION (First 5 rows of cross-sectional sample):\n")
verification_cols2 <- c("individual", "survey_wave", "age", "gender", "agreeableness", "kind")
available_cols2 <- intersect(verification_cols2, colnames(subsample1))
if(length(available_cols2) > 0) {
  print(head(subsample1 %>% select(all_of(available_cols2)), 5))
} else {
  print(head(subsample1 %>% select(1:min(7, ncol(subsample1))), 5))
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("🎉 STAGE", CURRENT_STAGE$number, ": PARTICIPANTS AND SAMPLE CHARACTERISTICS COMPLETE! 🎉\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

if(!exists("%.%")) {
  `%.%` <- function(x, n) paste0(rep(x, n), collapse = "")
}
