# Install and load necessary packages
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("shiny")) install.packages("shiny")
if (!require("CompQuadForm")) install.packages("CompQuadForm")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
install.packages("gridExtra")
library(gridExtra)

library(meta)
library(metadat)
library(readxl)
library(dplyr)
library(shiny)
library(metafor)
library(CompQuadForm)
library(RColorBrewer)


# Step 1: Import the Dataset
# Replace "D:/Dataset.xlsx" with the actual path to your dataset
dataset <- read_excel("F:/Meta_data_extraction.xlsx", sheet= "prevalence")
dataset


# Step 3: Perform Overall Meta-Analysis
overall_meta_analysis <- metaprop(
  event = dataset$Case,
  n = dataset$Samplesize,
  studlab = dataset$AuthorYear,
  data = dataset,
  sm = "PLO",           # Proportion (prevalence)
  method = "Inverse",
  method.tau = "REML",# DerSimonian and Laird method for tau²
  common = FALSE, 
  random = TRUE         # Use random-effects model
)
overall_meta_analysis
which(event < 0 | event != floor(event) | is.na(event))

# Step 4: Summarize the Overall Meta-Analysis
summary(overall_meta_analysis)


ci_color <- brewer.pal(9, "Greys")[6]  # Color for confidence intervals
individual_diamond_color <- "royalblue"  # Green for individual diamonds
pooled_diamond_color <- "red"  # Red for pooled effect diamond

# Save ultra-high-resolution PNG (1200 DPI, Increased Height)
png("forest_plot_1200dpi.png", width = 10, height = 12,res = 1200, units ="in" )
tiff("forest_plot_1200dpi.tiff", width = 8, height = 10, res = 1200,units = "in")

# Adjust margins to prevent cropping
par(mar = c(10,10,10,10))  # Increase bottom, left, top, right margins
par(mfrow = c(1,1))  # Reset layout to avoid multi-panel issues

# Generate the forest plot
forest(overall_meta_analysis, 
       leftcols = c("studlab"),  # Only show "Study" column
       leftlabs = c("Study"),  
       
       label.e = "Proportion",
       label.het = "Heterogeneity",
       colgap.forest.left = "5mm",
       
       # 🎨 Styling
       box.size = 0,  # Remove squares
       col.square = individual_diamond_color,  
       col.diamond = pooled_diamond_color,  
       col.diamond.lines = "black",  # Black outline for diamonds
       
       # Other styling
       digits = 3, 
       ci.width = 4, 
       line.width = 7, 
       hrzl.line = list("5mm", "blue"), 
       backtransf = TRUE,
       
       xlab = "Prevalence of Pneumonia",  
       cex = 4.5,  # Increased text size for better readability  
       cex.lab = 4.0,  
       cex.axis = 3.8, 
       print.tau2 = TRUE, 
       print.I2 = TRUE, 
       print.pval.Q = TRUE, 
       grid = TRUE, 
       col.grid = "blue")

dev.off()  # Close the graphics device and save the file

##################################

# Step 5: Perform Meta-Analysis by Subgroups
meta_region <- metaprop(
  event = dataset$Case,
  n = dataset$Samplesize,
  studlab = dataset$AuthorYear,
  data = dataset,
  sm = "PLO",           # Proportion (prevalence)
  method = "Inverse",
  method.tau = "REML",  # DerSimonian and Laird method for tau²
  common = FALSE, 
  random = TRUE,        # Use random-effects model
  subgroup = dataset$Region  # Subgroup by Setting
)

summary(meta_region)

# Subgroup analysis for Setting
meta_setting <- metaprop(
  event = dataset$Case,
  n = dataset$Samplesize,
  studlab = dataset$AuthorYear,
  data = dataset,
  sm = "PLO",           # Proportion (prevalence)
  method = "Inverse",
  method.tau = "REML",  # DerSimonian and Laird method for tau²
  common = FALSE, 
  random = TRUE,        # Use random-effects model
  subgroup = dataset$Setting  # Subgroup by Setting
)
# Summarize the results
summary(meta_setting)
# Subgroup analysis for Study_design
meta_design <- metaprop(
  event = dataset$Case,
  n = dataset$Samplesize,
  studlab = dataset$AuthorYear,
  data = dataset,
  sm = "PLO",           # Proportion (prevalence)
  method = "Inverse",
  method.tau = "REML",  # DerSimonian and Laird method for tau²
  common = FALSE, 
  random = TRUE,        # Use random-effects model
  subgroup = dataset$Design  # Subgroup by Study_design
)

# Summarize the results
summary(meta_design)
# Categorize sample size into subgroups
dataset <- dataset %>%
  mutate(
    SampleSize_Group = case_when(
      Samplesize < 500 ~ "Small",
      Samplesize >= 500 & Samplesize < 5000 ~ "Medium",
      Samplesize >= 5000 ~ "Large"
    )
  )

# Subgroup analysis for Sample Size
meta_sample_size <- metaprop(
  event = dataset$Case,
  n = dataset$Samplesize,
  studlab = dataset$AuthorYear,
  data = dataset,
  sm = "PLO",           # Proportion (prevalence)
  method = "Inverse",
  method.tau = "REML",  # DerSimonian and Laird method for tau²
  common = FALSE, 
  random = TRUE,        # Use random-effects model
  subgroup = dataset$SampleSize_Group  # Subgroup by Sample Size
)
# Summarize the results
summary(meta_sample_size)
# Forest plot for SampleSize_Group
png("subgroup_forest.png", height = 6, width = 9, res = 1200, units = "in")
forest_samplesize=forest(
  meta_sample_size, 
  layout = "subgroup",  # Display subgroups
  col.subgroup = "blue",  # Color for subgroup labels
  print.subgroup.labels = TRUE,  # Print subgroup labels
  spacing = 1.5,
  colgap.forest.left = "3cm",
  colgap.forest.right = "0.5cm",
  addrow=TRUE,
  addrows.below.overall=2,
  addrow.subgroups=TRUE
)

# Install and load necessary libraries
install.packages("magick")
install.packages("grid")
library(magick)
library(grid)
# Save Plot 1: Region (Blue Theme)
png("forest_region.png", height = 9, width = 10, res = 1200, units = "in")
forest(
  meta_region, 
  layout = "subgroup", 
  col.subgroup = "black", 
  col.square = "lightblue", 
  col.square.lines = "royalblue", 
  col.diamond = "royalblue", 
  col.diamond.lines = "darkblue", 
  print.subgroup.labels = TRUE, 
  spacing = 1.5,
  colgap.forest.left = "3cm",
  colgap.forest.right = "0.5cm",
  addrow = TRUE,
  addrows.below.overall = 2,
  addrow.subgroups = TRUE,
  main = "Region Subgroup"
)
dev.off()

# Save Plot 2: Setting (Red Theme)
png("forest_setting.png", height = 9, width = 10, res = 1200, units = "in")
forest(
  meta_setting, 
  layout = "subgroup", 
  col.subgroup = "black", 
  col.square = "yellow", 
  col.square.lines = "yellow", 
  col.diamond = "yellow", 
  col.diamond.lines = "darkred", 
  print.subgroup.labels = TRUE, 
  spacing = 1.5,
  colgap.forest.left = "3cm",
  colgap.forest.right = "0.5cm",
  addrow = TRUE,
  addrows.below.overall = 2,
  addrow.subgroups = TRUE,
  main = "Setting Subgroup"
)
dev.off()

# Save Plot 3: Design (Green Theme)
png("forest_design.png", height =9  , width = 10, res = 1200, units = "in")
forest(
  meta_design, 
  layout = "subgroup", 
  col.subgroup = "black", 
  col.square = "darkgray", 
  col.square.lines = "black", 
  col.diamond = "darkgreen", 
  col.diamond.lines = "darkgreen", 
  print.subgroup.labels = TRUE, 
  spacing = 1.5,
  colgap.forest.left = "3cm",
  colgap.forest.right = "0.5cm",
  addrow = TRUE,
  addrows.below.overall = 2,
  addrow.subgroups = TRUE,
  main = "Design Subgroup"
)
dev.off()

# Save Plot 4: Sample Size (Purple Theme)
png("forest_samplesize.png", height = 9, width = 10, res = 1200, units = "in")
forest(
  meta_sample_size, 
  layout = "subgroup", 
  col.subgroup = "black", 
  col.square = "lavender", 
  col.square.lines = "darkorchid", 
  col.diamond = "purple", 
  col.diamond.lines = "darkorchid", 
  print.subgroup.labels = TRUE, 
  spacing = 1.5,
  colgap.forest.left = "3cm",
  colgap.forest.right = "0.5cm",
  addrow = TRUE,
  addrows.below.overall = 2,
  addrow.subgroups = TRUE,
  main = "Sample Size Subgroup"
)
dev.off()
