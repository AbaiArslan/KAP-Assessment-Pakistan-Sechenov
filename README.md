This repository contains the analysis workflow for the thesis:

## Author
**Arslan Arshad**

## Repository Description
This repository is for the thesis titled:

> "A Cross-Sectional Assessment of Knowledge, Attitudes, And Practices (KAP) In the General Population of Pakistan and International Students of Sechenov University."

## Project Objective
To assess and compare antibiotic-related **Knowledge, Attitudes, and Practices (KAP)** among:
- The general population of Pakistan
- International students of Sechenov University

## Main Script
- `mph_thesis_analysis.R` — complete end-to-end analysis pipeline.

## Input Data
- Expected file: `mph_thesis.xlsx`
- Keep this file in the repository root before running the script.
- Raw data is excluded from version control via `.gitignore`.

## Outputs
Running the analysis script generates:
- Publication-ready tables in `.docx`
- NPG-styled figures in `.png`

## R Package Requirements
The script requires the following packages:

- `readxl`
- `dplyr`
- `tidyverse`
- `flextable`
- `officer`
- `ggplot2`
- `ggsci`
- `patchwork`
- `tidyr`

Install them with:

```r
install.packages(c(
  "readxl", "dplyr", "tidyverse", "flextable", "officer",
  "ggplot2", "ggsci", "patchwork", "tidyr"
))
```

## How to Run
1. Clone or download this repository.
2. Place `mph_thesis.xlsx` in the project root directory.
3. Open R/RStudio in the project folder.
4. Run:

```r
source("mph_thesis_analysis.R")
```

## Notes
- The script performs cleaning, scoring, subgroup comparison, and automated export of all tables/figures.
- Cohorts include:
  - Pakistani general population
  - Sechenov University students

## License
This project is licensed under the **MIT License**.  
See the [LICENSE](LICENSE) file for details.
