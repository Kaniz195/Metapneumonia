# Install required packages (if not installed)
# install.packages("gt")
# install.packages("dplyr")
# install.packages("webshot2")  # Required for saving gt tables as images

# Load necessary libraries
library(gt)
library(dplyr)
library(webshot2)  # Required for gtsave

# Define symbols
green_check <- "✅"  # Yes
red_cross <- "❌"    # No
yellow_question <- "❓"  # Unclear
not_applicable <- "🚫"  # Not Applicable

# Create the dataset
data <- data.frame(
  Study = 1:11,
  Author = c("Karki S et al.", "Ram et al.", "Shah et al.", "Nasrin et al.",
             "Hoang et al.", "Akahoshi et al.", "Islam et al.", "George et al.",
             "Sharma et al.", "Bhat & Manjunath", "Chen et al."),
  Q1 = c(1, 1, 1, 1, 1, 1, 1, 9, 1, 1, 1),
  Q2 = c(1, 1, 0, 0, 1, 1, 1, 9, 1, 1, 1),
  Q3 = c(0, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1),
  Q4 = c(0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1),
  Q5 = c(1, 1, 9, 1, 1, 1, 1, 1, 2, 1, 1),
  Q6 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  Q7 = c(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1),
  Q8 = c(1, 1, 0, 1, 1, 1, 0, 1, 2, 1, 1),
  Q9 = c(9, 1, 1, 9, 9, 1, 1, 9, 9, 1, 1),
  Q10 = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1),
  Score = c("7/11", "10/11", "6/11", "8/11", "9/11", "8/11", "9/11", "7/11", "4/11", "10/11", "10/11"),
  Overall = c("Moderate", "High", "Moderate", "High", "High", "High", "High", "Moderate", "Low", "High", "High")
)

# Convert values to symbols
data <- data %>%
  mutate(across(Q1:Q10, ~ case_when(
    . == 1 ~ green_check,      # Yes
    . == 0 ~ red_cross,        # No
    . == 9 ~ yellow_question,  # Unclear
    . == 2 ~ not_applicable,   # Not Applicable
    TRUE ~ as.character(.)
  )))

# Format the table using gt with proper spacing
gt_tbl <- data %>%
  gt() %>%
  tab_header(
    title = "JBI Checklist for Case-Control Study"
  ) %>% 
  cols_label(
    Study = "Study",
    Q1 = "Q1", Q2 = "Q2", Q3 = "Q3", Q4 = "Q4", 
    Q5 = "Q5", Q6 = "Q6", Q7 = "Q7", Q8 = "Q8",
    Q9 = "Q9", Q10 = "Q10",
    Overall = "Overall Rating",
    Score = "Score"
  ) %>%
  tab_options(
    table.font.size = "medium",
    data_row.padding = px(15),    # Increase row padding
    column_labels.padding = px(18),  # Increase header padding
    table.border.top.width = px(4),   # Adjust table border
    table.border.bottom.width = px(3)
  ) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "black", weight = px(1.2)),  # Add spacing between cells
    locations = cells_body()
  )

# Save the table as an image with proper width & height
gtsave(gt_tbl, filename = "jbi_case_control_study.png", expand = 50, vwidth = 1200, vheight = 800)
