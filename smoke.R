

# Step 1: Import the Dataset
# Replace "D:/Dataset.xlsx" with the actual path to your dataset
dataset <- read_excel("E:/Jaynal vai/Extraction.xlsx", sheet = "cigarette smoke")
dataset
# Step 2: Rename Variables for Clarity
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



