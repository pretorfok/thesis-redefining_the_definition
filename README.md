## README.md 

This repository contains the complete data processing and statistical analysis code for analysing coupled heart rate, sleep stage, and noise monitoring records. 

The entire pipeline runs automatically using relative paths provided by the R 'here' package. Once you place your raw data files into the correct folders, the code will run cleanly on any computer without needing any manual path adjustments.

---

## Folder Structure

```text
thesis-redefining_the_definition/
|-- thesis-redefining_the_definition.Rproj    # RStudio Project file
|-- run_all.R                                 # Master script
|-- README.md                                 # This setup guide
|-- scripts/                                  # Folder containing the 21 scripts
|-- data/
|   |-- raw/                                  # Original data files
|   |   |-- soundless-data-no-geo/            # Noise level and heart rate
|   |   |-- soundless-history/                # Session UUID to participant maps
|   |   \-- isruc-group-3/                    # Pre-cleaned clinical sleep files
|   \-- processed/                            # Intermediate/final data files
\-- outputs/                                  # Figures and text logs
    |-- figures/                              # Generated charts
    \-- All_Console_Outputs.txt               # Full execution log

```

## Required Packages

The pipeline relies on several open-source R libraries for data manipulation, rolling calculations, and visualizations. Before running 'run_all.R', ensure these are installed on your system.

| Library | Purpose |
| :--- | :--- |
| `data.table` | High-speed data manipulation and memory-efficient matrix operations |
| `ggplot2` | Vector graphic visualizations and diagnostic plotting |
| `ggrepel` | Non-overlapping label placement for phenotype scatterplots |
| `stringi` | Robust character string processing and filename regex parsing |
| `here` | Relative file pathway tracking and computational sandbox containment |
| `patchwork` | Composition of complex plots and layout alignment |
| `scales` | Scalable axis formatting and coordinate transformations |

To check and automatically install any missing dependencies, copy and paste the following code snippet directly into your RStudio console:

```r
required_pkgs <- c("data.table", "ggplot2", "ggrepel", "stringi", "here", "patchwork", "scales")
missing_pkgs  <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if(length(missing_pkgs) > 0) install.packages(missing_pkgs)
```
---

## Setup & Verification Guide

### 1. Open the Project

Open RStudio by double-clicking the **'thesis-redefining_the_definition.Rproj'** file in the root directory. This tells RStudio exactly where the project folder is located on your hard drive and configures all file paths to work automatically.

### 2. Verify Your Raw Data Files

The raw data sets must be placed in their respective folders before running the code. Please double-check that your files are dropped into these specific folders:

* **Noise & Heart Rate Records:** Confirm your continuous field logging data is sitting inside `data/raw/soundless-data-no-geo/`.
* **Participant History Sheets:** Confirm the extensionless participant text matching logs are inside `data/raw/soundless-history/`.
* **Clinical Reference Files:** Confirm that the pre-cleaned ISRUC hospital comparison files (`P1.csv` through `P10.csv`) are inside `data/raw/isruc-group-3/`.

### 3. Run the Automated Pipeline

To run the entire analysis from start to finish, open the master script **'run_all.R'** in your root directory and run/source it.

To run the entire analysis from start to finish, open the master script **'run_all.R'** in your root directory and run/source it.

This master script will automatically run the following 21 scripts in their required chronological order:

1. 'Step 1 -- Merge & De-deplicate.R'
2. 'Step 2 -- Ghost Removal.R'
3. 'Step 3 -- Conflict Resolve.R'
4. 'Step 4 -- Senor Data & Temporal Frequency Check.R'
5. 'Step 5 -- Participant Mapping & Reconciliation.R'
6. 'Step 5a -- Temporal Frequency Histogram.R'
7. 'Step 6 -- Temporal Trimming & Buffering.R'
8. 'Step 6a -- Sleep Statistics.R'
9. 'Step 7 -- Z-score Algorithm.R'
10. 'Step 8 -- L90 Algorithm.R'
11. 'Step 9 -- Rise Time Algorithm.R'
12. 'Step 10 -- Heart Rate Spike Predication.R'
13. 'Step 11 -- Sleep Stage Transition Predication.R'
14. 'Step 12 -- Graphic Comparison of 3 Algoirthms.R'
15. 'Step 13 -- All Descriptive Statistics.R'
16. 'Step 14 -- Individual Sensitivity Martrix.R'
17. 'Step 15 -- Clustering Maping and Phenotype Comparison.R'
18. 'Step 16 -- Clinic Sleep Quality vs Phenotype.R'
19. 'Step 17 -- ISRUC Reference Baseline & Decouopling Ratio.R'
20. 'Step 18 -- Soundless Sessional Baseline.R'
21. 'Step 19 -- Temporal Window Sensitivity Analysis.R'

---

## Generated Results & Deliverables

When the master `run_all.R` script finishes executing, all intermediate datasets, statistical reports, and visualisation plots are saved to the following paths:

### Processed Data (`data/processed/`)

| File Name | Purpose |
| :--- | :--- |
| `step_6_TRIMMED_ANALYSIS_DATASET.csv` | Data trimmed to sleep windows only. |
| `step_11_FINAL_MAPPED_DATA.csv` | Merged dataset with all noise flags (Z-score, L90+, Rise-Time). |
| `step_14_INDIVIDUAL_PROFILES_HOURLY.csv` | Heart/brain risk ratios per participant. |
| `step_16_CLINICAL_IMPACT_SUMMARY.csv` | Publication-ready sleep statistics. |

### Outputs (`outputs/`)

| File Name | Purpose |
| :--- | :--- |
| `All_Console_Outputs.txt` | Full execution log of the R terminal. |
| `figures/step_5a_histogram.png` | Sampling frequency quality control. |
| `figures/step_12_final_thesis_matrix.png` | Core comparative thesis bar chart. |
| `figures/step_15_Sensitivity_Map.png` | Participant clustering (Type A, B, or C). |
