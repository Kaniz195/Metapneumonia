
if (!require("janitor")) install.packages("janitor")
library(janitor)
library(meta)
library(readxl)
library(dplyr)
library(shiny)
library(metafor)
library(CompQuadForm)
library(RColorBrewer)


# Step 1: Import the Dataset
# Replace "D:/Dataset.xlsx" with the actual path to your dataset
dataset <- read_excel("F:/Md. Ashikuzzaman/NST- MSC Thesis/Data_Extraction_25.02.10_bad_deya_theke.xlsx", sheet = "RiskGender")
dataset

dataset <- clean_names(dataset)
dataset
colnames(dataset)

# Step 2: Rename Variables for Clarity
# Assuming your dataset has columns "Positive", "Total", "AuthorYear", and "SubGroupVariable"
dataset <- dataset %>%
  rename(
    Study = author_year,       # Study label (Author & Year)
    RiskFactor = factor_name,  # Associated risk factor
    OR = odds_ratio,           # Odds Ratio
    LowerCI = ci_lower,        # Lower bound of 95% CI
    UpperCI = ci_upper         # Upper bound of 95% CI
  ) %>%
  mutate(
    OR = as.numeric(OR),           # Ensure OR is numeric
    LowerCI = as.numeric(LowerCI), # Ensure LowerCI is numeric
    UpperCI = as.numeric(UpperCI)  # Ensure UpperCI is numeric
  )
View(dataset)



# Step 3: Perform Overall Meta-Analysis
meta_risk_factors <- metagen(
  TE = dataset$OR,               # Effect size (Odds Ratio)
  lower = dataset$LowerCI,       # Lower confidence interval
  upper = dataset$UpperCI,       # Upper confidence interval
  studlab = dataset$Study,       # Study names
  data = dataset,
  sm = "OR",                     # Summary measure: Odds Ratio
  method.tau = "DL",             # DerSimonian and Laird method for tau²
  random = TRUE,                 # Use random-effects model
  common = FALSE                 # Do not use fixed-effect model
)
meta_risk_factors

# Step 4: Summarize the Overall Meta-Analysis
summary(meta_risk_factors)

# Step 5: Perform Meta-Analysis by Subgroups
subgroup_meta_risk <- metagen(
  TE = dataset$OR,
  lower = dataset$LowerCI,
  upper = dataset$UpperCI,
  studlab = dataset$Study,
  data = dataset,
  sm = "OR",
  method.tau = "DL",
  common = FALSE, 
  random = TRUE, 
  subgroup = dataset$RiskFactor  # Subgroup analysis by Risk Factor
)
subgroup_meta_risk

# Step 6: Summarize the Subgroup Meta-Analysis
summary(subgroup_meta_analysis)

# Step 7: Generate Forest Plots
# Forest plot for overall meta-analysis


#Main_Code
forest(meta_risk_factors, 
       leftcols = c("studlab", "RiskFactor"), # Show study & risk factor in the left column
       leftlabs = c("Study", "Risk Factor"),
       xlab = "Odds Ratio",
       col.diamond = "blue",   # Blue diamond for pooled effect
       col.diamond.lines = "black", # Black outline for the pooled effect diamond
       digits = 3, 
       ci.width = 2, 
       line.width = 2, 
       cex = 1.2,  
       cex.lab = 1.3,  
       cex.axis = 1.2, 
       print.tau2 = TRUE, 
       print.I2 = TRUE, 
       print.pval.Q = TRUE, 
       grid = TRUE, 
       col.grid = "gray90")


# Forest plot for subgroup meta-analysis
forest(subgroup_meta_analysis, 
       leftcols = c("studlab", "event", "n"), 
       leftlabs = c("Study", "Positive", "Total"), 
       label.e = "Proportion", 
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

