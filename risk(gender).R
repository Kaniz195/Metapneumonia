# Install and load necessary packages
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("shiny")) install.packages("shiny")
if (!require("CompQuadForm")) install.packages("CompQuadForm")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
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
dataset <- read_excel("E:/Jaynal vai/Extraction.xlsx", sheet = "Gender")
dataset

library(dplyr)

# Rename columns and ensure numeric conversion
dataset <- dataset %>%
  rename(
    Study = AuthorYear,       # Study label (Author & Year)
    RiskFactor = Category,    # Associated risk factor
    OR = `OR/HR`,             # Odds Ratio (use backticks for special characters)
    LowerCI = `CI(lower)`,    # Lower bound of 95% CI (use backticks for special characters)
    UpperCI = `CI(upper)`     # Upper bound of 95% CI (use backticks for special characters)
  ) %>%
  mutate(
    OR = as.numeric(OR),           # Ensure OR is numeric
    LowerCI = as.numeric(LowerCI), # Ensure LowerCI is numeric
    UpperCI = as.numeric(UpperCI)  # Ensure UpperCI is numeric
  )

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
       col.grid = "yellow")
png("forest_region.png", height = 9, width = 10, res = 1200, units = "in")
forest(
  meta_risk_factors, 
  leftcols = c("studlab", "RiskFactor"), # Show study & risk factor in the left column
  leftlabs = c("Study", "Risk Factor"),
  xlab = "Odds Ratio", 
  col.subgroup = "blue", 
  col.square = "royalblue", 
  col.square.lines = "darkblue", 
  col.diamond = "blue", 
  col.diamond.lines = "darkblue", 
  print.subgroup.labels = TRUE, 
  spacing = 1.5,
  colgap.forest.left = "3cm",
  colgap.forest.right = "0.5cm",
  addrow = TRUE,
  addrows.below.overall = 2,
  addrow.subgroups = TRUE,
  main = "Risk of Gender"
)
dev.off()


