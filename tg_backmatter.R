library(readxl)
library(dplyr)
library(ggplot2)



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
#Glossary (Y/N)
################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    glossary = ifelse(glossary %in% c("Y", "U"), "Yes", "No"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

# Count by dissertation status
glossary_counts <- tibetic_grammars %>%
  group_by(is_diss, glossary) %>%
  summarise(Count = n(), .groups = 'drop')

print(status_counts)

# Count by period, data collection mention, and dissertation status
period_glossary <- tibetic_grammars %>%
  group_by(period, glossary, is_diss) %>%
  summarise(Count = n(), .groups = 'drop')

print(period_glossary)


# Bar plot for time period with stacked 'Yes' and 'No', and separate bars for Dissertation and Published Grammar
period_glossary_plot <- ggplot(period_glossary, aes(x = period, y = Count, fill = glossary)) +
  geom_bar(stat = "identity", position = position_dodge2(width = .75, preserve = "single"), aes(group = interaction(is_diss, glossary)), width = .75) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "Presence of glossary",
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
print(period_glossary_plot)


################################################
#Index (Y/N)
################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocessing the data
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    index = ifelse(index %in% c("Y", "U"), "Yes", "No"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late")),
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    is_diss = factor(is_diss, levels = c("Dissertation", "Published Grammar"))
  )

# Count by dissertation status
index_counts <- tibetic_grammars %>%
  group_by(is_diss, index) %>%
  summarise(Count = n(), .groups = 'drop')

print(status_counts)

# Count by period, data collection mention, and dissertation status
period_index <- tibetic_grammars %>%
  group_by(period, index, is_diss) %>%
  summarise(Count = n(), .groups = 'drop')

print(period_index)


# Bar plot for time period with stacked 'Yes' and 'No', and separate bars for Dissertation and Published Grammar
period_index_plot <- ggplot(period_index, aes(x = period, y = Count, fill = index)) +
  geom_bar(stat = "identity", position = position_dodge2(width = .75, preserve = "single"), aes(group = interaction(is_diss, index)), width = .75) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "green"),
                    name = "Presence of index",
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
print(period_index_plot)
