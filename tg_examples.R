###########################################################################################################
# EXAMPLES
###########################################################################################################
library(readxl)
library(dplyr)
library(ggplot2)
  

################################################
#Interlinear Glossed Examples (Y/N)
################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    example_ig = ifelse(example_ig %in% c("Y"), "Yes", "No"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

# Count by dissertation status
example_ig_counts <- tibetic_grammars %>%
  group_by(is_diss, example_ig) %>%
  summarise(Count = n(), .groups = 'drop')

print(example_ig_counts)

# Count by period, data collection mention, and dissertation status
period_example_ig <- tibetic_grammars %>%
  group_by(period, example_ig, is_diss) %>%
  summarise(Count = n(), .groups = 'drop')

print(period_example_ig)


# Bar plot for time period with stacked 'Yes' and 'No', and separate bars for Dissertation and Published Grammar
period_example_ig_plot <- ggplot(period_example_ig, aes(x = period, y = Count, fill = example_ig)) +
  geom_bar(stat = "identity", position = position_dodge2(width = .75, preserve = "single"), aes(group = interaction(is_diss, example_ig)), width = .75) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "Examples interlinear glossed",
                    labels = c("No","Yes")) +
  facet_wrap(~is_diss, scales = "free_x", nrow = 1) +
  labs(x = "Period of publishing/defense", y = "Count of grammars") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::label_number(auto = TRUE)) +  # Adjust to show integer values
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
print(period_example_ig_plot)

#########
#Number of tiers in the examples
#########

# Ensure that the n_tier column is numeric
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    n_tiers = as.numeric(n_tiers)  # Convert total_years_grammar to numeric if it's not already
  )

print(table(tibetic_grammars$n_tiers))  # Check the distribution of n_tiers 



################################################
#Is native orthography(ies) used in the examples (Y/N)
################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    nat_orth = ifelse(nat_orth %in% c("Y"), "Yes", "No"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

# Count by dissertation status
nat_orth_counts <- tibetic_grammars %>%
  group_by(is_diss, nat_orth) %>%
  summarise(Count = n(), .groups = 'drop')

print(nat_orth_counts)

# Count by period, data collection mention, and dissertation status
period_nat_orth <- tibetic_grammars %>%
  group_by(period, nat_orth, is_diss) %>%
  summarise(Count = n(), .groups = 'drop')

print(period_nat_orth)


# Bar plot for time period with stacked 'Yes' and 'No', and separate bars for Dissertation and Published Grammar
period_nat_orth_plot <- ggplot(period_nat_orth, aes(x = period, y = Count, fill = nat_orth)) +
  geom_bar(stat = "identity", position = position_dodge2(width = .75, preserve = "single"), aes(group = interaction(is_diss, nat_orth)), width = .75) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "Native orthography(ies) in the examples",
                    labels = c("No","Yes")) +
  facet_wrap(~is_diss, scales = "free_x", nrow = 1) +
  labs(x = "Period of publishing/defense", y = "Count of grammars") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::label_number(auto = TRUE)) +  # Adjust to show integer values
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
print(period_nat_orth_plot)

################################################
#Use of Leipzig glossing rules (Y/N)
################################################

##############################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
# Filtering out 'N/A' values from 'leipzig' and adjusting the 'leipzig' values
tibetic_grammars <- tibetic_grammars %>%
  filter(leipzig != "N/A") %>%
  mutate(
    leipzig = ifelse(leipzig == "Y", "Yes", "No"),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

# Summarise the data after filtering
leipzig_summary <- tibetic_grammars %>%
  group_by(is_diss, leipzig) %>%
  summarise(Count = n(), .groups = 'drop')

print(leipzig_summary)

# Bar plot for dissertation status
leipzig_plot <- ggplot(leipzig_summary, aes(x = is_diss, y = Count, fill = leipzig)) +
  geom_bar(stat = "identity", position = position_dodge(), width = .5) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "Use of Leipzig glossing rules",
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
print(leipzig_plot)


################################################
#List of abbreviations present (Y/N)
################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    abbreviations = ifelse(abbreviations %in% c("Y"), "Yes", "No"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

# Count by dissertation status
abbreviations_counts <- tibetic_grammars %>%
  group_by(is_diss, abbreviations) %>%
  summarise(Count = n(), .groups = 'drop')

print(abbreviations_counts)

# Count by period, data collection mention, and dissertation status
period_abbreviations <- tibetic_grammars %>%
  group_by(period, abbreviations, is_diss) %>%
  summarise(Count = n(), .groups = 'drop')

print(period_abbreviations)


# Bar plot for time period with stacked 'Yes' and 'No', and separate bars for Dissertation and Published Grammar
period_abbreviations_plot <- ggplot(period_abbreviations, aes(x = period, y = Count, fill = abbreviations)) +
  geom_bar(stat = "identity", position = position_dodge2(width = .75, preserve = "single"), aes(group = interaction(is_diss, abbreviations)), width = .75) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "List of abbreviations",
                    labels = c("No","Yes")) +
  facet_wrap(~is_diss, scales = "free_x", nrow = 1) +
  labs(x = "Period of publishing/defense", y = "Count of grammars") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = scales::label_number(auto = TRUE)) +  # Adjust to show integer values
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
print(period_abbreviations_plot)


###################################################
#source of data v/s count of grammars: Venn Diagram
###################################################

library(readxl)
library(dplyr)
library(VennDiagram)
library(grid)

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocess and categorize data sources
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    E = grepl("E", example_source),
    NS = grepl("NS", example_source),
    OS = grepl("OS", example_source),
    NM = grepl("NM", example_source)
  )

# Separate data into dissertations and published grammars
dissertations <- filter(tibetic_grammars, is_diss == "Dissertation")
published_grammars <- filter(tibetic_grammars, is_diss == "Published Grammar")

# Function to create Venn diagram
create_venn <- function(data, title) {
  indices <- list(
    E = which(data$E),
    NS = which(data$NS),
    OS = which(data$OS),
    NM = which(data$NM)
  )
  
  venn.plot <- venn.diagram(
    x = indices,
    filename = NULL,
    category.names = c("E", "NS", "OS", "NM"),
    fill = c("lightblue", "pink", "yellow", "lightgreen"),
    alpha = 0.5,
    label.col = "black",
    cex = 1.5,
    fontface = "bold",
    cat.cex = 2,
    cat.col = c("skyblue", "salmon", "goldenrod", "green"),
    cat.fontface = "bold",
    cat.default.pos = "outer",
    cat.dist = c(0.06, 0.06, 0.1, 0.09),
    lwd = 2,
    main.title = title,
    main.cex = 2
  )
  
  # Plot the Venn diagram
  grid.draw(venn.plot)
}

# Create Venn diagrams for each subset
create_venn(dissertations, "Venn Diagram for Dissertations")
create_venn(published_grammars, "Venn Diagram for Published Grammars")

###################################################
#Bar plot of sources of data across different periods of publishing/defense

library(readxl)
library(dplyr)
library(ggplot2)

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocess data to categorize data sources
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    Source = case_when(
      grepl("E", example_source) & grepl("NS", example_source) & grepl("OS", example_source) ~ "E+NS+OS",
      grepl("E", example_source) & grepl("NS", example_source) ~ "E+NS",
      grepl("E", example_source) & !grepl("NS", example_source) & !grepl("OS", example_source) ~ "E",
      grepl("NS", example_source) & !grepl("E", example_source) & !grepl("OS", example_source) ~ "NS",
      !grepl("NS", example_source) & !grepl("E", example_source) & !grepl("OS", example_source) ~ "NM",
      TRUE ~ as.character(example_source)  # catch any other formats as they are
    )
  ) %>%
  group_by(period, is_diss, Source) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the bar plot with abbreviated legend labels
example_source_plot <- ggplot(tibetic_grammars, aes(x = period, y = Count, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", width = 0.9), width = 1) +  # Adjust bar width and dodge width
  scale_fill_brewer(palette = "Set3", name = "Data Source",
                    labels = c("E" = "E", "E+NS" = "E+NS", "NS" = "NS", 
                               "E+NS+OS" = "E+NS+OS", "NM" = "NM")) +
  facet_wrap(~is_diss, scales = "free_y") +
  labs(x = "Period of publishing/defense", y = "Count of grammars") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::label_number(auto = TRUE)) +  # Adjust to show integer values
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
print(example_source_plot)




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
    y = "Average Likert scale scores"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
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
    x = "Example citation (Likert scale scores)",
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
