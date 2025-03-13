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
not_applicable <- "🚫"  # Not Applicable symbol

# Create the dataset
data <- data.frame(
  Study = 1:13,
  Author = c("Goyal et al.", "Kasundriya et al.", "Ooi et al.", "Kosai et al.", "Chen et al.",
             "Shan et al.", "Nguyen et al.", "Ramachandran", "Nguyen et al.", "Miao et al.",
             "Murray et al.", "Tran et al.", "Vijay et al."),
  Q1 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  Q2 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  Q3 = c(1, 1, 9, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1),
  Q4 = c(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 9, 0),
  Q5 = c(1, 1, 1, 1, 1, 1, 9, 1, 1, 1, 1, 1, 2),
  Q6 = c(1, 1, 1, 1, 0, 0, 1, 1, 2, 1, 1, 1, 1),
  Q7 = c(1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1),
  Q8 = c(9, 9, 9, 1, 2, 1, 1, 1, 9, 9, 9, 9, 2),
  Q9 = c(9, 0, 1, 1, 0, 2, 0, 9, 1, 9, 1, 2, 2),
  Q10 = c(9, 2, 2, 0, 2, 2, 0, 0, 0, 9, 9, 2, 1),
  Q11 = c(1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1),
  Overall = c("High", "High", "High", "High", "Moderate", "High", "High", "High", "High", "Moderate", "High", "Moderate", "High"),
  Score = c("8/11", "8/11", "8/11", "9/11", "6/11", "8/11", "8/11", "9/11", "9/11", "7/20", "8/11", "7/11", "8/11")
)

# Convert values to symbols
data <- data %>%
  mutate(across(Q1:Q11, ~ case_when(
    . == 1 ~ green_check,   # ✅ for 1
    . == 0 ~ red_cross,     # ❌ for 0
    . == 9 ~ yellow_question,  # ❓ for 9
    . == 2 ~ not_applicable,  # 🚫 for 2
    is.na(.) ~ "",  # Keep NA values as empty
    TRUE ~ as.character(.)
  )))


# Format the table using gt with proper spacing
gt_tbl <- data %>%
  gt() %>%
  tab_header(
    title = "Quality Assessment Table for Cohort Studies",
    #subtitle = "Using ✅, ❌, ❓, and 🚫 Symbols"
  ) %>% 
  cols_label(
    Study = "Study",
    Q1 = "Q1", Q2 = "Q2", Q3 = "Q3", Q4 = "Q4", 
    Q5 = "Q5", Q6 = "Q6", Q7 = "Q7", Q8 = "Q8",
    Q9 = "Q9", Q10 = "Q10", Q11 = "Q11",
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
gtsave(gt_tbl, filename = "quality_assessment_table_with_na.png", expand = 60, vwidth = 1200, vheight = 800)
