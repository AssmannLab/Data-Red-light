install.packages("ggVennDiagram")


# Load libraries
library(ggplot2)
library(ggVennDiagram)

# Example data for each category - replace these sets with your actual data
upregulated_arabisopsis <- c("Metabolite1", "Metabolite2", "Metabolite3") # replace with actual upregulated data
upregulated_vicia <- c("Metabolite2", "Metabolite3", "Metabolite4")       # replace with actual upregulated data
unchanged_arabisopsis <- c("Metabolite5", "Metabolite6")                  # replace with actual unchanged data
unchanged_vicia <- c("Metabolite6", "Metabolite7")                        # replace with actual unchanged data
downregulated_arabisopsis <- c("Metabolite8", "Metabolite9")              # replace with actual downregulated data
downregulated_vicia <- c("Metabolite9", "Metabolite10")                   # replace with actual downregulated data

# Convert each list to a named list for ggVennDiagram
upregulated_data <- list(`A. thaliana` = upregulated_arabisopsis, `V. faba` = upregulated_vicia)
unchanged_data <- list(`A. thaliana` = unchanged_arabisopsis, `V. faba` = unchanged_vicia)
downregulated_data <- list(`A. thaliana` = downregulated_arabisopsis, `V. faba` = downregulated_vicia)

# Plotting Venn diagrams with large fonts
font_size <- 7 # Adjust as needed for size

# Upregulated Venn Diagram
ggVennDiagram(upregulated_data) +
  ggtitle("Upregulated Metabolites") +
  theme(plot.title = element_text(size = font_size, face = "bold"),
        legend.text = element_text(size = font_size),
        plot.margin = margin(5, 5, 5, 5))

# Unchanged Venn Diagram
ggVennDiagram(unchanged_data) +
  ggtitle("Unchanged Metabolites") +
  theme(plot.title = element_text(size = font_size, face = "bold"),
        legend.text = element_text(size = font_size),
        plot.margin = margin(5, 5, 5, 5))

# Downregulated Venn Diagram
ggVennDiagram(downregulated_data) +
  ggtitle("Downregulated Metabolites") +
  theme(plot.title = element_text(size = font_size, face = "bold"),
        legend.text = element_text(size = font_size),
        plot.margin = margin(5, 5, 5, 5))
