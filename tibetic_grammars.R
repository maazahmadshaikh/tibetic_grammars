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


###################################################
#source of data v/s count of grammars: Venn Diagram
###################################################

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

################################################
# METHODOLOGY
################################################

################################################
#Methodology LS v/s Publication Period: Box Plot
################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Ensure that 'period' and 'is_diss' columns are correctly formatted as factors
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(`is_diss` == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar")),
    `methodology_LS` = as.numeric(`methodology_LS`)  # Ensure this is numeric for plotting
  )

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Create a box plot
ggplot_object <- ggplot(tibetic_grammars, aes(x = period, y = `methodology_LS`, fill = is_diss)) +
  geom_boxplot(position = position_dodge2(preserve = "single"), width = 1) +  # Set a fixed width for all boxes
  scale_fill_manual(values = c("Dissertation" = "lightblue", "Published Grammar" = "lightgreen"),
                    name = "Type of grammar",
                    labels = c("Dissertation", "Published grammar")) +
  labs(
    x = "Period of publishing/defense",
    y = "Average Likert scale score for methodology"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),  # Increase font size for x-axis labels
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),  # Increase space between axis labels and title
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),  # Increase space between axis labels and title
  )

# Print the plot
print(ggplot_object)


#####################################
#Methodology LS v/s count of grammars
#####################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(`is_diss` == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar")),
    `methodology_LS` = as.numeric(`methodology_LS`)  # Ensure this is numeric for grouping
  )

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Creating a summary table for plotting
likert_counts <- tibetic_grammars %>%
  group_by(`methodology_LS`, is_diss) %>%
  summarise(count = n(), .groups = 'drop')

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Create a bar plot
bar_plot <- ggplot(likert_counts, aes(x = as.factor(`methodology_LS`), y = count, fill = is_diss)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", width = 0.9), width = 0.5) +  # Adjust bar width and dodge width
  scale_fill_manual(values = c("Dissertation" = "lightblue", "Published Grammar" = "lightgreen"),
                    name = "Type of grammar",
                    labels = c("Dissertation", "Published grammar")) +
  labs(
    x = "Methodology (Likert scale score)",
    y = "Count of grammars"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10))
  )

# Print the plot
print(bar_plot)


###########################################################################################################
# EXAMPLES
###########################################################################################################

#####################################################
#Example citation LS v/s Publication Period: Box Plot
#####################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Ensure that 'period' and 'is_diss' columns are correctly formatted as factors
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(`is_diss` == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar")),
    `example_citation_LS` = as.numeric(`example_citation_LS`)  # Ensure this is numeric for plotting
  )

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Create a box plot
ggplot_object <- ggplot(tibetic_grammars, aes(x = period, y = `example_citation_LS`, fill = is_diss)) +
  geom_boxplot(position = position_dodge2(preserve = "single"), width = 1) +  # Set a fixed width for all boxes
  scale_fill_manual(values = c("Dissertation" = "lightblue", "Published Grammar" = "lightgreen"),
                    name = "Type of grammar",
                    labels = c("Dissertation", "Published grammar")) +
  labs(
    x = "Period of publishing/defense",
    y = "Average Likert scale score for example citation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),  # Increase font size for x-axis labels
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),  # Increase space between axis labels and title
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),  # Increase space between axis labels and title
  )

# Print the plot
print(ggplot_object)

#####################################
#Example citation LS v/s count of grammars
#####################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(`is_diss` == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar")),
    `example_citation_LS` = as.numeric(`example_citation_LS`)  # Ensure this is numeric for grouping
  )

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Creating a summary table for plotting
likert_counts <- tibetic_grammars %>%
  group_by(`example_citation_LS`, is_diss) %>%
  summarise(count = n(), .groups = 'drop')

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Create a bar plot
bar_plot <- ggplot(likert_counts, aes(x = as.factor(`example_citation_LS`), y = count, fill = is_diss)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", width = 0.9), width = 0.5) +  # Adjust bar width and dodge width
  scale_fill_manual(values = c("Dissertation" = "lightblue", "Published Grammar" = "lightgreen"),
                    name = "Type of grammar",
                    labels = c("Dissertation", "Published grammar")) +
  labs(
    x = "Example citation (Likert scale score)",
    y = "Count of grammars"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10))
  )

# Print the plot
print(bar_plot)

###########################################################################################################
# TEXTS
###########################################################################################################


#####################################################
#Texts LS v/s Publication Period: Box Plot
#####################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Ensure that 'period' and 'is_diss' columns are correctly formatted as factors
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(`is_diss` == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar")),
    `texts_LS` = as.numeric(`texts_LS`)  # Ensure this is numeric for plotting
  )

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Create a box plot
ggplot_object <- ggplot(tibetic_grammars, aes(x = period, y = `texts_LS`, fill = is_diss)) +
  geom_boxplot(position = position_dodge2(preserve = "single", width = .5), width = .75) +  # Set a fixed width for all boxes
  scale_fill_manual(values = c("Dissertation" = "lightblue", "Published Grammar" = "lightgreen"),
                    name = "Type of grammar",
                    labels = c("Dissertation", "Published grammar")) +
  labs(
    x = "Period of publishing/defense",
    y = "Average Likert scale score for texts"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 12, face = "bold"),  # Increase font size for x-axis labels
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),  # Increase space between axis labels and title
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),  # Increase space between axis labels and title
  )

# Print the plot
print(ggplot_object)

#####################################
#Texts LS v/s count of grammars
#####################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(`is_diss` == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar")),
    `texts_LS` = as.numeric(`texts_LS`)  # Ensure this is numeric for grouping
  )

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Creating a summary table for plotting
likert_counts <- tibetic_grammars %>%
  group_by(`texts_LS`, is_diss) %>%
  summarise(count = n(), .groups = 'drop')

# Double-check data entries for each category
print(table(tibetic_grammars$is_diss))

# Create a bar plot
bar_plot <- ggplot(likert_counts, aes(x = as.factor(`texts_LS`), y = count, fill = is_diss)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", width = 0.9), width = 0.5) +  # Adjust bar width and dodge width
  scale_fill_manual(values = c("Dissertation" = "lightblue", "Published Grammar" = "lightgreen"),
                    name = "Type of grammar",
                    labels = c("Dissertation", "Published grammar")) +
  labs(
    x = "Texts (Likert scale score)",
    y = "Count of grammars"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10))
  )

# Print the plot
print(bar_plot)



################################################
# BINARY VARIABLES  (Y/N)
################################################

################################################
#Time collecting data
################################################


##########################
# Bar plot for time period
###########################
library(readxl)
library(dplyr)
library(ggplot2)

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    data_coll_time_mention = ifelse(data_coll_time_mention %in% c("Y", "U"), "Yes", "No"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

# Count by period, data collection mention, and dissertation status
period_mention_diss_counts <- tibetic_grammars %>%
  group_by(period, data_coll_time_mention, is_diss) %>%
  summarise(Count = n(), .groups = 'drop')

print(period_mention_diss_counts)

# Bar plot for time period with stacked 'Yes' and 'No', and separate bars for Dissertation and Published Grammar
period_plot <- ggplot(period_mention_diss_counts, aes(x = period, y = Count, fill = data_coll_time_mention)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.9, preserve = "single"), aes(group = interaction(is_diss, data_coll_time_mention)), width = .5) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "Mention of data collection time",
                    labels = c("No","Yes")) +
  facet_wrap(~is_diss, scales = "free_x", nrow = 1) +
  labs(x = "Period of publishing/defense", y = "Count of grammars") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# Print the plot
print(period_plot)

##############################
# Count by dissertation status
status_counts <- tibetic_grammars %>%
  group_by(is_diss, data_coll_time_mention) %>%
  summarise(Count = n(), .groups = 'drop')

print(status_counts)

# Bar plot for dissertation status
status_plot <- ggplot(status_counts, aes(x = is_diss, y = Count, fill = data_coll_time_mention)) +
  geom_bar(stat = "identity", position = position_dodge(), width = .5) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "Mention of data collection time",
                    labels = c("No","Yes")) +
  labs(x = "Type of grammar", y = "Count of grammars") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::label_number(auto = TRUE)) +  # Adjust to show integer values
  theme_minimal()+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# Print the plot for Dissertation Status
print(status_plot)

################################################
#Find out the number of U in the data_coll_time_mention" column

unclear_counts <- tibetic_grammars %>%
  filter(data_coll_time_mention == "U") %>%
  summarise(Count = n())

print(unclear_counts)

#Generate a sumamry of the items in the data_coll_time_mention column
print(table(tibetic_grammars$data_coll_time_mention))

# Generate counts of endangered status per branch
endangered_counts <- tibetic_grammars %>%
  group_by(branch, endg) %>%
  summarise(Count = n(), .groups = 'drop')



# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    data_coll_time_mention = ifelse(data_coll_time_mention %in% c("Y", "U"), "Yes", "No"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

