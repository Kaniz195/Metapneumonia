# Install and load necessary libraries
install.packages("readxl")  # For reading Excel files
install.packages("meta")    # For meta-analysis
library(readxl)
library(meta)
library(metadat)
# Load your Excel file
data <- read_excel("F:/Meta_data_extraction.xlsx", sheet= "prevalence")

# Ensure 'Prevalence' column is numeric
data$Prevalence <- as.numeric(data$Prevalence)

# Calculate the standard error (SE) for each study
# SE = sqrt(p * (1 - p) / n), where p is prevalence and n is sample size
data$SE <- sqrt(data$Prevalence * (1 - data$Prevalence) / data$Samplesize)

# Perform meta-analysis using the 'metagen' function from the 'meta' package
meta_analysis <- metagen(
  TE = data$Prevalence,       # Effect size (prevalence)
  seTE = data$SE,             # Standard error of the effect size
  studlab = paste(data$Author, data$Year),  # Study labels (e.g., "Smith 2010")
  sm = "PLOGIT",              # Logit transformation for better stability
  method.tau = "REML",
  common = FALSE, 
  random = TRUE# Random-effects model using Restricted Maximum Likelihood
)

# Print the results of the meta-analysis
print(meta_analysis)
ci_color <- brewer.pal(9, "Greys")[6]  # Color for confidence intervals
individual_diamond_color <- "green"  # Green for individual diamonds
pooled_diamond_color <- "red"  # Red for pooled effect diamond

# Save ultra-high-resolution PNG (1200 DPI, Increased Height)
png("forest_plot_1200dpi.png", width = 10000, height = 12000, res = 1200)
tiff("forest_plot_1200dpi.tiff", width = 10000, height = 12000, res = 1200)

# Adjust margins to prevent cropping
par(mar = c(5,5,5,5))  # Increase bottom, left, top, right margins
par(mfrow = c(1,1))  # Reset layout to avoid multi-panel issues
forest(meta_analysis, 
       leftcols = c("studlab"),  # Only show "Study" column
         
       
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
dev.off()
meta_region <- metagen(
  TE = data$Prevalence,       # Effect size (prevalence)
  seTE = data$SE,             # Standard error of the effect size
  studlab = paste(data$Author, data$Year),  # Study labels (e.g., "Smith 2010")
  sm = "PLOGIT",              # Logit transformation for better stability
  method.tau = "REML",
  common = FALSE, 
  random = TRUE,# Random-effects model using Restricted Maximum Likelihood
  subgroup = data$Region
)
summary(meta_region)
# Forest plot for Region
forest(meta_region, 
       layout = "subgroup",  # Display subgroups
       col.subgroup = "blue",  # Color for subgroup labels
       print.subgroup.labels = TRUE)  # Print subgroup labels
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
meta_study_design <- metaprop(
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
summary(meta_study_design)