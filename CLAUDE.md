# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains a Shiny application for sarcoma data visualization and analysis. The application provides interactive visualization of gene expression data across different datasets (GTEx, PCAWG) and analyzes copy number variations in relation to gene expression.

## Data Structure

The application loads various datasets from a local directory (`~/Dropbox/SS/data`), including:

1. GTEx expression data (tissue-specific gene expression)
2. PCAWG cancer data (gene expression across tumor types)
3. Copy number variation data
4. Membrane protein information
5. Genomic tracks and visualization data

## Application Components

1. **Main Interface**: A Shiny app with sidebar controls for gene selection and data source options
2. **Visualization Tabs**:
   - GTEx: Gene expression distribution across tissues with patient sample highlighted
   - PCAWG: Gene expression distribution across cancer types with patient sample highlighted
   - Copy-Number/Expression: Scatter plot comparing copy numbers vs. gene expression
   - Copy-Number: Genomic visualization of copy number data for selected gene
   - Membrane GTEx: Comparison of tumor vs. normal expression for membrane proteins

## Running the Application

To run the Shiny application:

```r
# From R console
source("app.R")

# From command line
Rscript -e "shiny::runApp('app.R')"

# Using the shiny package
R -e "shiny::runApp('.')"
```

Make sure the `data` directory is located in the same directory as `app.R`.

## Development Notes

1. The app has been optimized to load only necessary libraries and use namespace calls where appropriate:
   - Fully loaded libraries: `shiny`, `ggplot2`, `plotly` (used extensively throughout the code)
   - Libraries accessed via namespace: `data.table`, `GenomicRanges` (used for specific functions)
   - Conditionally loaded: `gTrack` (only loaded if available)

2. Two versions of the app exist:
   - `app.R`: Main application
   - `app_claude.R`: Alternative/duplicate version

3. Data dependencies must be present in the `~/Dropbox/SS/data` directory, including:
   - `gtex_melt.rds`
   - `pcawg_data.rds`
   - `gr_rna.rds`
   - `rna_data.rds`
   - `tumor_vs_blood.cnvs.txt`
   - `dt_mem.rds`
   - `dt_cn_expression.rds`
   - `membrane_gtex.rds`
   - `td38_gencode_gtrack.rds` (for genomic track visualization)
   - `tumor_vs_blood.tumour_tumourLogR.txt`

4. The code has been modified to load `genes_granges` from the data directory
5. Modified to handle missing `tumor_vs_blood.tumour_tumourLogR.txt` file gracefully

## Data Exploration

When exploring the codebase, note that most of the data manipulation and visualization logic is contained within the server function's reactive expressions. Key data structures include:

1. `dt.gtex.melt`: GTEx gene expression data
2. `dt.pcawg.melt`: PCAWG cancer expression data
3. `dt_cn_exp`: Copy number vs. expression data
4. `memgtex`: Membrane protein expression data