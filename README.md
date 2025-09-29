# HILDA Personality Measures Validation Study

[![DOI](https://img.shields.io/badge/DOI-10.xxxx/xxxx-blue)](https://doi.org/10.17605/osf.io/6gkrq)
[![OSF](https://img.shields.io/badge/OSF-Project-blue)](https://osf.io/px9gf)

## Overview

This repository contains the complete analysis pipeline for the paper:

**"Longitudinal measurement invariance and stability of Big Five personality traits: Evidence from Australian panel data"**

*Authors: Gisela Roth and Sebastian Roth*  
*Institutions: The University of Western Australia; University of Lausanne*

## Abstract

This study provides a comprehensive psychometric evaluation of Big Five personality measures in the Household, Income and Labour Dynamics in Australia (HILDA) Survey across 16 years (2005-2021). Using advanced factor analytic methods including exploratory structural equation modeling (ESEM), we demonstrate strong factorial validity, measurement invariance, and temporal stability of personality trait measures in a large longitudinal sample.

## Key Findings

- **Strong factorial validity**: Custom ESEM implementation (CFI = .749) outperformed traditional CFA (.716)
- **Robust measurement invariance**: Full strict invariance achieved across 16 years
- **Excellent temporal stability**: 4-year correlations .60-.78, declining gradually to .53-.67 over 16 years
- **High internal consistency**: Cronbach's α .74-.82 across all Big Five factors
- **Comprehensive validation**: 28 personality items validated across 5 waves with N > 9,000

## Repository Structure

```
├── README.md                     # This file
├── manuscript_replication_code.R              # Main analysis controller
├── scripts/                     # Analysis modules
│   ├── prepare_hilda_data.R    # HILDA data preparation
│   ├── participants_and_measures.R    # Sample creation & cleaning
│   ├── preliminary_analyses.R # Sample characteristics
│   ├── factor_structure_analysis.R   # Model comparisons & factor correlation
│   ├── reliability_analysis.R # Internal consistency & test-retest
│   ├── measurement_invariance_analysis.R # Longitudinal invariance testing
│   ├── validity_analysis.R # Convergent, discriminant, & criterion validity
│   └── robustness_analysis.R  # Cross-validation & outlier analysis
├── data/                        # Data directory
│   ├── raw/ # HILDA data files (user-provided)
│   └── hilda-dataset.qs        # Prepared HILDA dataset
├── output/                      # Analysis results
│   ├── data/                 # Processed data files
│   ├── participants/                # Participants and measures results
│   ├── preliminary/                # Preliminary analyses results
│   ├── factor_structure/                # Factor structure analysis results
│   ├── reliability/                # Reliability analysis results
│   ├── measurement_invariance/                # Measurement invariance analysis results
│   ├── validity/                # Validity analysis results
│   ├── robustness/                # Robustness analysis results
│   └── logs/                # Execution logs
├── manuscript/                  # Manuscript files
│   ├── manuscript.tex          # Main manuscript
│   ├── tables/                 # LaTeX tables
│   └── figures/        # LaTeX figures
├── supplemental materials/                  # Supplemental Materials files
│   ├── supplement.tex          # supplemental materials
│   ├── tables/                 # LaTeX tables
│   └── figures/        # LaTeX figures
├── renv/               # Reproducibility library
├── renv.lock               # Package versions
└── environment/                 # Reproducibility environment
    └── session_info.txt        # R session information
```

## Requirements

### Software Requirements

- **R** (≥ 4.0.0)
- **RStudio** (recommended)

### R Package Dependencies

```r
# Core analysis packages
install.packages(c(
  "tidyverse",      # Data manipulation and visualization
  "lavaan",         # Structural equation modeling  
  "psych",          # Psychometric analysis
  "semTools",       # SEM utilities
  "rprojroot",      # Project directory management
  "qs",             # Fast R serialization
  "haven",          # Import Stata files
  "data.table",     # Fast data manipulation
  "VIM",            # Missing data visualization
  "mice",           # Multiple imputation
  "fs"              # File system operations
))
```

### Data Requirements

This analysis requires access to the **HILDA Survey** dataset (2005-2021 waves). The HILDA Survey is administered by the Melbourne Institute and requires separate data access approval.

**To obtain HILDA data:**
1. Visit: https://melbourneinstitute.unimelb.edu.au/hilda
2. Complete data access application
3. Download Combined_*.dta files for analysis waves

## Quick Start

### 1. Setup Project

```r
# Clone/download this repository
# Open R project file in RStudio
# Install required packages (see Requirements above)
```

### 2. Prepare Data

```r
# Update data path in prepare_data.R
DATA_CONFIG$hilda_data_path <- "/path/to/your/HILDA/data"

# Run data preparation
source("scripts/prepare_hilda_data.R")
```

### 3. Run Analysis

```r
# Option A: Complete analysis pipeline
source("manuscript_replication_code.R")
run_complete_analysis()

# Option B: Individual modules
source("scripts/participants_and_measures.R")
source("scripts/preliminary_analyses.R")
# ... continue with other modules
```

## Key Methodological Innovations

### Custom ESEM Implementation

Our study features a sophisticated **custom ESEM implementation** that successfully models personality data where standard software approaches fail:

```r
# Two-step ESEM procedure:
# 1. EFA with oblimin rotation to identify optimal loading pattern
# 2. SEM specification using EFA loadings as starting values
# 3. Cross-loadings ≥|.10| freely estimated, <|.10| constrained to zero
```

This approach:
- **Outperformed traditional CFA** (ΔCFI = .033)
- **Addressed personality item complexity** through theoretically meaningful cross-loadings
- **Succeeded where native software failed** due to model complexity (28 items × 5 factors)

### Comprehensive Temporal Stability Assessment

- **All possible wave combinations** tested (4-year through 16-year intervals)
- **Declining stability pattern** consistent with personality development theory
- **Excellent long-term stability** even over 16-year period

## Results Summary

| Analysis Component | Key Finding |
|-------------------|-------------|
| **Factor Structure** | Clear Big Five structure with ESEM superior to traditional CFA |
| **Model Comparison** | Bifactor (.847) > ESEM (.749) > CFA (.716) > Hierarchical (.702) |
| **Internal Consistency** | α = .74-.82 across factors and waves |
| **Temporal Stability** | 4-year: r = .60-.78; 16-year: r = .53-.67 |
| **Measurement Invariance** | Full strict invariance across 16 years |
| **Sample Validation** | N = 9,959 (Wave 5) to N = 17,494 (Wave 21) |

## Citation

If you use this code or findings in your research, please cite:

```
Roth, G and Roth, S. (2025). Longitudinal measurement invariance and stability of Big Five personality traits: Evidence from Australian panel data. https://doi.org/
```

## Data Access and Ethics

This study uses data from the HILDA Survey. The HILDA Project was initiated and is funded by the Australian Government Department of Social Services (DSS) and is managed by the Melbourne Institute of Applied Economic and Social Research (Melbourne Institute). The findings and views reported in this analysis should not be attributed to DSS or the Melbourne Institute.

**Data Access**: HILDA data requires separate application and approval. See: https://melbourneinstitute.unimelb.edu.au/hilda

**Ethics**: This analysis uses de-identified survey data under approved research protocols.

## Contributing

This repository documents a completed research project. For questions or suggestions:

- **Open an issue** for technical problems
- **Contact authors** for methodological questions
- **Fork repository** for extensions or replications

## License

**Code**: MIT License - see LICENSE file for details  
**Data**: HILDA data subject to separate license terms  
**Manuscript**: Copyright restrictions apply

## Contact

**Corresponding Author**: Sebastian Roth
**Email**: sebastian.roth@unil.ch
**Institution**: University of Lausanne

---

## Reproducibility Information

### R Session Information
```r
R version 4.5.1
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.6.1

Package versions used:
- lavaan_0.6-20
- psych_2.5.6
- tidyverse_2.0.0
[Additional package versions in environment/pipeline_execution_log.txt]
```

### Computational Environment
This analysis was conducted using R with the `renv` package for dependency management. The complete computational environment can be restored using:

```r
renv::restore()
```

**Last Updated**: September 2025
