### Reproducible Research Fundamentals -  Main R Script

# Load libraries ---- 

# Load necessary libraries
library(haven)  # for reading .dta files
library(dplyr)  # for data manipulation
library(tidyr)  # for reshaping data
library(stringr) # work with strings
library(labelled) # use labels
library(gtsummary) # tables
library(gt) # tables
library(ggplot2) #graphs
library(tidyverse) # working with tidy data
library(modelsummary) # creating summary tables
library(stargazer) # writing nice tables
library(RColorBrewer) # color palettes

# Recover environment ---- 
# Only run in the first time 
# renv::restore()

# Set data path ----

# this is the second root of the project, the first root is the code whose directory 
# is already being handled by the rstudio project.

data_path <- "your path here"

# Run the R scripts ----
a <- Sys.time()
# Clean Data
source("Code/01-processing-data.R")

# Construction
source("Code/02-constructing-data.R")

# Analyses
source("Code/03-analyzing-data.R")

print(paste0("Time:", Sys.time() - a)) 
