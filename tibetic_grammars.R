setwd("D:/01_Projects/01_PhD/03_Analysis/02_TGR/tibetic_grammars")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the data, assuming the first row is now properly set as column names
tibetic_grammars <- read_excel("Tibetic_Grammars_Review_Master.xlsx", skip = 0)

# Define the desired order for branches
branch_order <- c("NW", "W", "C", "SW", "S", "SE", "E", "NE")

# Generate counts of endangered status per branch
endangered_counts <- tibetic_grammars %>%
  group_by(branch, endg) %>%
  summarise(Count = n(), .groups = 'drop')

# Set the factor levels for branch and endg to ensure the correct plotting order
# Ensure that 'Endangered' appears first in the factor to place it at the bottom of the stack
endangered_counts$branch <- factor(endangered_counts$branch, levels = branch_order)
endangered_counts$endg <- factor(endangered_counts$endg, levels = c("Y", "N"),
                                 labels = c("Endangered", "Not endangered"))

# Create the stacked bar plot with specified colors for each category
ggplot(endangered_counts, aes(x = branch, y = Count, fill = endg)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Endangered" = "red", "Not endangered" = "blue")) +
  labs(x = "Branch of Tibetic", y = "Number of Grammars", fill = "Legend") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )


# Generate counts of endangered status per year for the given period and dissertation type
year_distribution_1 <- tibetic_grammars %>%
  filter(year >= 1831 & year <= 1950) %>%
  group_by(year, `is-diss`) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the plot
ggplot(year_distribution_1, aes(x = year, y = Count, fill = `is-diss`)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_continuous(breaks = seq(1830, 1950, by = 10)) +  # Adjusted for 10-year intervals for better detail
  scale_y_continuous(breaks = seq(0, max(year_distribution_1$Count, na.rm = TRUE), by = 1)) +
  scale_fill_manual(values = c("Y" = "blue", "N" = "green", "R" = "red"),
                    labels = c("Dissertations", "Published Grammars", "Revised Editions"),
                    name = "Type of Work") +
  theme_minimal() +
  labs(x = "Year of publication/defense", y = "Count of grammars") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")


# Second Plot: 1951 to 2000
year_distribution_2 <- tibetic_grammars %>%
  filter(year >= 1951 & year <= 2000) %>%
  group_by(year, `is-diss`) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the stacked bar plot with thinner bars
ggplot(year_distribution_2, aes(x = year, y = Count, fill = `is-diss`)) +
  geom_bar(stat = "identity", position = "stack", width = 0.3) +  # Reduced bar width
  scale_x_continuous(breaks = seq(1950, 2000, by = 2)) +
  scale_y_continuous(breaks = seq(0, max(year_distribution_2$Count, na.rm = TRUE), by = 1)) +  # Adjusted to max of year_distribution_2
  scale_fill_manual(values = c("Y" = "blue", "N" = "green", "R" = "red"),
                    labels = c("Published grammars", "Dissertations", "Revised Editions"),
                    name = "Type of Work") +  # Adjust legend labels and add title
  theme_minimal() +
  labs(x = "Year of publication/defense", y = "Count of grammars") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"  # Move the legend to the bottom
  )


# Third Plot: 2001 to 2024
year_distribution_3 <- tibetic_grammars %>%
  filter(year >= 2001 & year <= 2024) %>%
  group_by(year, `is-diss`) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the stacked bar plot
ggplot(year_distribution_3, aes(x = year, y = Count, fill = `is-diss`)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) + # Changed to stack position for stacking bars
  scale_y_continuous(breaks = seq(0, max(year_distribution_3$Count, na.rm = TRUE), by = 1)) +
  scale_x_continuous(breaks = 2001:2024, labels = as.character(2001:2024)) + # Explicitly set x-axis breaks for 2001-2024
  scale_fill_manual(values = c("Y" = "blue", "N" = "green", "R" = "red"),
                    labels = c("Dissertations", "Published Grammars", "Revised Editions"),
                    name = "Type of Work") +
  theme_minimal() +
  labs(x = "Year of publication/defense",
       y = "Count of grammars") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")



# Load necessary libraries
library(readxl)
library(VennDiagram)

# Read the data
tibetic_grammars <- read_excel("Tibetic_Grammars_Review_Master.xlsx")

# Preprocess and merge OF and OF-ES
tibetic_grammars <- tibetic_grammars %>%
  mutate(OF = grepl("OF", `data-source`),
         PS = grepl("PS", `data-source`),
         US = grepl("US", `data-source`),
         NS = grepl("NS", `data-source`))

# Convert logicals to indices for Venn diagram
indices <- list(
  OF = which(tibetic_grammars$OF),
  PS = which(tibetic_grammars$PS),
  US = which(tibetic_grammars$US),
  NS = which(tibetic_grammars$NS)
)

# Create a Venn diagram
venn.plot <- venn.diagram(
  x = indices,
  filename = NULL,
  category.names = c("OF", "PS", "US", "NS"),
  fill = c("lightblue", "pink", "yellow", "lightgreen"),
  alpha = .5,
  label.col = "black",
  cex = 1.5,
  fontface = "bold",
  cat.cex = 2,
  cat.col = c("skyblue", "salmon", "goldenrod", "green"),
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.dist = c(0.06, 0.06, 0.1, 0.09),
  lwd = 2
)
# Plot the diagram
grid.draw(venn.plot)



