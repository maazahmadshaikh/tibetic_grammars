setwd("D:/01_Projects/01_PhD/03_Analysis/02_TGR/tibetic_grammars")

# Print current working directory
print(getwd())

# List files in the current directory
list.files()


library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the data, assuming the first row is now properly set as column names
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

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

#################################################
# Counts of grammar per year for the given period
#################################################

# First Plot: 1831 to 1950

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







################################################
# BINARY VARIABLES  (Y/N)
################################################

