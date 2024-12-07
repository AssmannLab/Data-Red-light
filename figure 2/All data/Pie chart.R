

data <- read.csv(choose.files(), header = TRUE)
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
data <- read_csv("path/to/your/Pie chart R.csv")  # Replace with your actual file path

# Standardize the 'Index' names if necessary
data$Index <- recode(data$Index, "GC/TOF-MS" = "GC TOF-MS")

# Create a combined dataset by summarizing across all indices
combined_data <- data %>%
  group_by(Category) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Index = "Combined")

# Add the combined data back to the original dataset
data_combined <- bind_rows(data, combined_data)

# Calculate percentages for each Category within each Index
data_combined <- data_combined %>%
  group_by(Index) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Specify the order of the levels in the Index factor to control plot arrangement
data_combined$Index <- factor(data_combined$Index, levels = c("GCMS-TQ8040", "QTRAP 4000", "GC TOF-MS", "Combined"))

# Define a color palette
generic_palette <- scales::hue_pal()(length(unique(data_combined$Category)))

# Plot pie charts for each index without numbers
ggplot(data_combined, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Full pie chart with white borders
  coord_polar(theta = "y") +
  facet_wrap(~ Index, nrow = 1) +
  scale_fill_manual(values = generic_palette) +  # Apply the generic color palette
  labs(fill = "") +
  theme_void() +
  theme(
    strip.text = element_text(size = 14)
  )
