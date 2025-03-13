# Install and load necessary packages
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("shiny")) install.packages("shiny")
if (!require("CompQuadForm")) install.packages("CompQuadForm")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

library(meta)
library(readxl)
library(dplyr)
library(shiny)
library(metafor)
library(CompQuadForm)
library(RColorBrewer)


# Step 1: Import the Dataset
# Replace "D:/Dataset.xlsx" with the actual path to your dataset
dataset <- read_excel("F:/Md. Ashikuzzaman/NST- MSC Thesis/Data_Extraction_25.02.10_bad_deya_theke.xlsx", sheet = "Prevalence")
dataset

# Step 2: Rename Variables for Clarity
# Assuming your dataset has columns "Positive", "Total", "AuthorYear", and "SubGroupVariable"
dataset <- dataset %>%
  rename(
    PositiveCases = CaseSize,      # Number of positive cases
    TotalSamples = AdolescentsSampleSize,          # Total number of samples
    Study = AuthorYear,            # Study label (e.g., Author and Year)
    Subgroup = Continent    # Subgroup variable (e.g., Sex)
  )
dataset

# Step 3: Perform Overall Meta-Analysis
overall_meta_analysis <- metaprop(
  event = dataset$PositiveCases,
  n = dataset$TotalSamples,
  studlab = dataset$Study,
  data = dataset,
  sm = "PLO",           # Proportion (prevalence)
  method.tau = "DL",    # DerSimonian and Laird method for tau²
  common = FALSE, 
  random = TRUE         # Use random-effects model
)
overall_meta_analysis

# Step 4: Summarize the Overall Meta-Analysis
summary(overall_meta_analysis)

# Step 5: Perform Meta-Analysis by Subgroups
subgroup_meta_analysis <- metaprop(
  event = dataset$PositiveCases,
  n = dataset$TotalSamples,
  studlab = dataset$Study,
  data = dataset,
  sm = "PLO",
  method.tau = "DL",
  common = FALSE, 
  random = TRUE, 
  byvar = dataset$Subgroup  # Subgroup analysis by host type
)

# Step 6: Summarize the Subgroup Meta-Analysis
summary(subgroup_meta_analysis)

# Step 7: Generate Forest Plots
# Forest plot for overall meta-analysis


ci_color <- brewer.pal(9, "Greys")[6]
individual_diamond_color <- "#D8BFD8"  # Light purple for individual diamonds
pooled_diamond_color <- "red"  # Red for pooled effect diamond


#Main_Code
forest(overall_meta_analysis, 
       leftcols = c("studlab"),  # Only show "Study" column
       leftlabs = c("Study"),  
       
       label.e = "Proportion",
       label.het = "Heterogeneity",
       colgap.forest.left = "5mm",
       
       # 🎨 Styling
       box.size = 0,  # Remove squares
       col.square = diamond_color,  # Light purple diamonds for studies
       col.diamond = pooled_diamond_color,  # Red diamond for pooled effect
       col.diamond.lines = "black",  # Black outline for diamonds
       
       # Other styling
       digits = 4, 
       ci.width = 2, 
       line.width = 3, 
       hrzl.line = list("3mm", "gray70"), 
       backtransf = TRUE,
       
       xlab = "Prevalence of Obesity",  
       cex = 1.9,  
       cex.lab = 1.3,  
       cex.axis = 1.2, 
       print.tau2 = TRUE, 
       print.I2 = TRUE, 
       print.pval.Q = TRUE, 
       grid = TRUE, 
       col.grid = "gray90")

#
forest(overall_meta_analysis, 
       leftcols = c("studlab", "event", "n"), 
       leftlabs = c("Study", "Positive", "Total"), 
       label.e = "Proportion", 
       box.size = "50",
       lab.het = "Heterogeneity",
       colgap.forest.left = "5mm")


forest(overall_meta_analysis, 
       leftcols = c("studlab", "event", "n"), 
       leftlabs = c("Study", "Positive", "Total"), 
       lab.e = "Proportion", 
       lab.het = "Heterogeneity",
       colgap.forest.left = "1mm",
       col.square = "gray", col.diamond = "red", col.diamond.lines = "black",
       xlim = c(0, 1), xlab = "Proportion of Obesity", fontsize = 12,
       cex.lab = 1, cex.axis = 1, print.tau2 = TRUE, print.I2 = TRUE, 
       print.pval.Q = TRUE, digits = 4, grid = TRUE, col.grid = "gray90",
       psize = TRUE)



# Forest plot for subgroup meta-analysis
forest(subgroup_meta_analysis, 
       leftcols = c("studlab", "event", "n"), 
       leftlabs = c("Study", "Positive", "Total"), 
       lab.e = "Proportion", 
       lab.het = "Heterogeneity",
       colgap.forest.left = "5mm")

# Step 8: Assess Publication Bias
# Funnel plot for overall meta-analysis
funnel(overall_meta_analysis)

# Egger's test for publication bias
metabias(overall_meta_analysis)

# Notes:
# 1. Ensure your dataset contains the correct columns for PositiveCases, TotalSamples, Study, and Subgroup.
# 2. Replace the file path and sheet name with your dataset's actual details.

