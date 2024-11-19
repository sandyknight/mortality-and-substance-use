#!/bin/bash

# Set the root project directory
PROJECT_DIR="/c/Users/sandy/projects/mortality_and_lyl_substance_misuse"
cd "$PROJECT_DIR" || exit

# Define directories to move files into
mkdir -p data/raw
mkdir -p data/processed
mkdir -p docs
mkdir -p output
mkdir -p plots
mkdir -p R

# Move files based on their type or folder designation

# Move raw data files
find . -type f -name "*.csv" -path "*/data/raw/*" -exec mv {} data/raw/ \;
find . -type f -name "*.xlsx" -path "*/data/raw/*" -exec mv {} data/raw/ \;
find . -type f -name "*.parquet" -path "*/data/raw/*" -exec mv {} data/raw/ \;

# Move processed data files
find . -type f -name "*.csv" -path "*/data/processed/*" -exec mv {} data/processed/ \;

# Move plot files
find . -type f -name "*.png" -exec mv {} plots/ \;

# Move documentation and template files
find . -type f -name "*.md" -exec mv {} docs/ \;
find . -type f -name "*.Rmd" -exec mv {} docs/ \;
find . -type f -name "*.qmd" -exec mv {} docs/ \;

# Move presentation and document outputs
find . -type f -name "*.pptx" -exec mv {} output/ \;
find . -type f -name "*.docx" -exec mv {} output/ \;

# Move R scripts
find . -type f -name "*.R" -exec mv {} R/ \;

# Remove empty directories
find . -type d -empty -delete

# Remove unnecessary files by pattern (edit as needed)
rm -f directory_structure_mortality_project.txt
rm -f temp_output_script_working.R

# Provide feedback on actions taken
echo "Project cleanup and organization complete."
