# Install required packages (if not installed)
#install.packages("gt")
#install.packages("dplyr")
#install.packages("webshot2")  # Required for saving gt tables as images

# Load necessary libraries
library(gt)
library(dplyr)
library(webshot2)  # Required for gtsave

# Define symbols
green_check <- "✅"  # Green circle with check
red_cross <- "❌"    # Red circle with cross
yellow_question <- "❓"  # Yellow circle with question mark

# Create the dataset
data <- data.frame(
  Study = 1:8,
  Author = c("Saha et al.", "Naheed et al.", "Gothankar et al.", "Shrestha et al.",
             "Luong et al.", "Bhurtel et al.", "Chang et al.", "Norbäck et al."),
  Q1 = c(1, 1, 1, 1, 1, 1, 1, 1),
  Q2 = c(1, 1, 1, 1, 9, 1, 1, 1),
  Q3 = c(1, 1, 0, 1, 1, 1, 0, 0),
  Q4 = c(1, 1, 0, 1, 1, 1, 0, 1),
  Q5 = c(1, 1, 1, 1, 9, 0, 1, 0),
  Q6 = c(1, 1, 0, 1, 1, 0, 1, 1),
  Q7 = c(1, 1, 0, 1, 1, 1, 1, 0),
  Q8 = c(1, 1, 0, 1, 0, 0, 1, 1),
  Overall = c("High", "High", "Low", "High", "Moderate", "Moderate", "Moderate", "Moderate"),
  Score = c("1", "1", "3/8", "1", "5/8", "5/8", "6/8", "5/8")  # Add Score column
)

# Convert values to symbols
data <- data %>%
  mutate(across(Q1:Q8, ~ case_when(
    . == 1 ~ green_check,
    . == 0 ~ red_cross,
    . == 9 ~ yellow_question,
    TRUE ~ as.character(.)
  )))

# Format the table using gt with proper spacing
gt_tbl <- data %>%
  gt() %>%
  tab_header(
    title = "Quality Assessment Table for Cross-Sectional study",
    #subtitle = "Using ✅, ❌, and ❓ Symbols"
  ) %>% 
  cols_label(
    Study = "Study",
    Q1 = "Q1", Q2 = "Q2", Q3 = "Q3", Q4 = "Q4", 
    Q5 = "Q5", Q6 = "Q6", Q7 = "Q7", Q8 = "Q8",
    Overall = "Overall Rating",
    Score = "Score"
  ) %>%
  tab_options(
    table.font.size = "medium",
    data_row.padding = px(20),    # Increase row padding
    column_labels.padding = px(22),  # Increase header padding
    table.border.top.width = px(6),   # Adjust table border
    table.border.bottom.width = px(4)
  ) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "black", weight = px(1.5)),  # Add spacing between cells
    locations = cells_body()
  )

# Save the table as an image with proper width & height
gtsave(gt_tbl, filename = "quality_assessment_table.png", expand = 50, vwidth = 1200, vheight = 800)
