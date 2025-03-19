# Performance Drift Analysis

This repository hosts **Drift_Code_Github.R**, an R script demonstrating methods and analyses used to investigate **performance drift** in the context of risk or outcome assessments. 

The associated publication can be found here:  
[**Performance drift in dynamic risk assessment: results from an integrated tool used in forensic mental health services**](https://pmc.ncbi.nlm.nih.gov/articles/PMC8902871/).


## Overview

- **Paper Reference**  
  - **Title**: Performance drift in a mortality prediction algorithm among patients with cancer during the SARS-CoV-2 pandemic  
  - **Authors**: (Parikh RB, Zhang Y, Kolla L, Chivers C, Courtright KR, Zhu J, Navathe AS, Chen J.)  
  - **Journal**: (Journal of the American Medical Informatics Association)  
  - **Year**: (2022)  
  - **PMC Link**: https://academic.oup.com/jamia/article/30/2/348/6835770

This R script supports or replicates the analyses described in the paper, focusing on **how performance drift occurs** over time or across different measurement points in dynamic risk assessment scenarios. The approach can be adapted to other domains where performance consistency is critical.


## How to Use This Repository

1. **Clone or Download**  
   - **Clone**:  
     ```bash
     git clone https://github.com/HumanAlgorithmCollaborationLab/Performance-Drift.git
     ```
   - **Download ZIP**:  
     - Click the green **Code** button → **Download ZIP**.

2. **Install Dependencies**  
   - You’ll need **R** (version 4.0+ recommended).  
   - Review **Drift_Code_Github.R** for any required packages (e.g., `tidyverse`, `dplyr`, `ggplot2`, etc.).  
   - Install them via:
     ```r
     install.packages(c("tidyverse","ggplot2","dplyr"))  # example
     ```
   - Adjust according to the script’s specific library calls.

3. **Run the Script**  
   - Open **Drift_Code_Github.R** in RStudio or another R environment.  
   - If required, update any data file paths or parameters inside the script.  
   - Execute the script to reproduce the analysis, generate figures, or view summary statistics.
