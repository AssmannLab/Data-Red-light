# Install and load the required packages
if (!requireNamespace("tidyheatmaps", quietly = TRUE)) {
  install.packages("tidyheatmaps")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}

library(tidyheatmaps)
library(dplyr)
library(stringr)

# Load your dataset
data <- read.csv(choose.files(), header = TRUE)

# Remove leading and trailing whitespaces in the Category column
data$Category <- str_trim(data$Category)

# Function to clean metabolite names by removing numbers and single letters at the beginning
clean_metabolite_name <- function(metabolite) {
  metabolite <- str_trim(metabolite)  # Remove any leading/trailing whitespace
  # Remove leading numbers or a single letter followed by whitespace or punctuation
  metabolite <- str_replace(metabolite, "^[0-9A-Za-z]{1}[^A-Za-z0-9]", "")
  return(metabolite)
}

# Apply the cleaning function to the Metabolite column
data$Cleaned_Metabolite <- sapply(data$Metabolite, clean_metabolite_name)

# Function to plot heatmap for a single category
plot_category_heatmap <- function(category_name) {
  # Filter the data for the specific category and order by the cleaned metabolite names
  category_data <- data %>%
    filter(Category == category_name) %>%
    arrange(Cleaned_Metabolite)
  
  # Make sure there's data left after filtering
  if (nrow(category_data) == 0) {
    cat("No data available for the specified category. Check the category name or data.\n")
    cat("Available categories are:\n")
    print(unique(data$Category))
    stop("Please use one of the available category names.")
  }
  
  # Plot the heatmap using tidyheatmap
  tidyheatmap(
    category_data,
    rows = Metabolite,
    columns = Species,
    values = Score,
    scale = "none"  # No scaling applied; adjust as needed
  ) +
    labs(title = paste("Category:", category_name),
         x = "Species",
         y = "Metabolite") +
    theme(axis.text.x = element_text(face = "italic", angle = 45, hjust = 1))  # Italicize species names
}

# Example usage: Plot heatmap for "CARBOHYDRATES"
plot_category_heatmap("PHENOLICS")
