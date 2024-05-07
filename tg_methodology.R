################################################
# METHODOLOGY
################################################
library(readxl)
library(dplyr)
library(ggplot2)


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

###################################################
#source of data v/s count of grammars: Venn Diagram
###################################################

library(readxl)
library(VennDiagram)
library(dplyr)
library(grid)  # For grid.draw()

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Preprocess and categorize data sources
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    OF = grepl("OF", `data_source`),
    PS = grepl("PS", `data_source`),
    US = grepl("US", `data_source`),
    NS = grepl("NS", `data_source`)
  )

# Separate data into dissertations and published grammars
dissertations <- filter(tibetic_grammars, is_diss == "Dissertation")
published_grammars <- filter(tibetic_grammars, is_diss == "Published Grammar")

# Function to create Venn diagram
create_venn <- function(data, title) {
  indices <- list(
    OF = which(data$OF),
    PS = which(data$PS),
    US = which(data$US),
    NS = which(data$NS)
  )
  
  venn.plot <- venn.diagram(
    x = indices,
    filename = NULL,
    category.names = c("OF", "PS", "US", "NS"),
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
      grepl("OF", data_source) & grepl("PS", data_source) & grepl("US", data_source) ~ "OF+PS+US",
      grepl("OF", data_source) & grepl("PS", data_source) ~ "OF+PS",
      grepl("OF", data_source) & grepl("US", data_source) ~ "OF+US",
      grepl("PS", data_source) & grepl("US", data_source) ~ "PS+US",
      grepl("OF", data_source) & !grepl("PS", data_source) & !grepl("US", data_source) ~ "OF",
      !grepl("OF", data_source) & !grepl("PS", data_source) & !grepl("US", data_source) ~ "NS",
      TRUE ~ as.character(data_source)  # catch any other formats as they are
    )
  ) %>%
  group_by(period, is_diss, Source) %>%
  summarise(Count = n(), .groups = 'drop')

# Create the bar plot with abbreviated legend labels
source_plot <- ggplot(tibetic_grammars, aes(x = period, y = Count, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single", width = 0.9), width = 1) +  # Adjust bar width and dodge width
  scale_fill_brewer(palette = "Set3", name = "Data Source",
                    labels = c("OF" = "OF", "OF+PS" = "OF+PS", "OF+US" = "OF+US", 
                               "OF+PS+US" = "OF+PS+US", "PS+US" = "PS+US", "NS" = "NS")) +
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
print(source_plot)


################################################
#Information on IERB/Ethics Protocol (Y/N)
################################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Process the data
ethics_by_type <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar")
  ) %>%
  group_by(is_diss, irb_ethics_info) %>%
  summarise(Count = n(), .groups = 'drop')

#print the summary
print(ethics_by_type)

# Plot for IRB/Ethics information by type of grammar
ethics_type_plot <- ggplot(ethics_by_type, aes(x = is_diss, y = Count, fill = irb_ethics_info)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_fill_manual(values = c("Y" = "green", "N" = "red"),
                    name = "IRB/ethics protocol information present",
                    labels = c("Yes" = "Contains Info", "No" = "No Info")) +
  labs(title = "IERB/Ethics information by type of grammar",
       x = "Type of Grammar", y = "Count of grammars") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text.x = element_text( size= 12, face = "bold", hjust = 1),  # Adjust text angle for better readability if necessary
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

# Print the plot
print(ethics_type_plot)


################################################
# Process the data for period of publishing/defense
ethics_by_period <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    period = factor(period, levels = c("E", "M", "L"), labels = c("Early", "Middle", "Late"))
  ) %>%
  group_by(period, is_diss, irb_ethics_info) %>%
  summarise(Count = n(), .groups = 'drop')

# Plot for IERB/Ethics information by period
ethics_period_plot <- ggplot(ethics_by_period, aes(x = period, y = Count, fill = irb_ethics_info)) +
  geom_bar(stat = "identity",position = position_dodge2(width = 0.9, preserve = "single"), aes(group = interaction(is_diss, irb_ethics_info)), width = .5) +
  scale_fill_manual(values = c("Y" = "green", "N" = "red"),
                    name = "IERB/ethics protocol information present",
                    labels = c("Yes" = "Contains Info", "No" = "No Info")) +
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
print(ethics_period_plot)



################################################
#Time collecting data mention (Y/N)
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

################################################

#Generate a summary of the items in the data_coll_time_mention column
print(table(tibetic_grammars$data_coll_time_mention))

#Generate a summary of the item in the column data_coll_time_months
print(table(tibetic_grammars$data_coll_time_months))

#Calculate the average of the values in the column data_coll_time_months
tibetic_grammars$data_coll_time_months <- as.numeric(tibetic_grammars$data_coll_time_months)

mean(tibetic_grammars$data_coll_time_months, na.rm = TRUE)

#Convert this answer into weeks
mean(tibetic_grammars$data_coll_time_months, na.rm = TRUE) * 4


########################################
#Mention of the year of first field trip
########################################

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Analyze the 'year_first_fw' column
year_mention_summary <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    Year_Mentioned = ifelse(year_first_fw == "NM", "No", "Yes")
  ) %>%
  group_by(is_diss, Year_Mentioned) %>%
  summarise(Count = n(), .groups = 'drop')

# View the summarized data
print(year_mention_summary)


################################################
#Total time spent towards the completion of grammars since the pilot study
################################################
library(readxl)
library(dplyr)
library(ggplot2)

# Load the data
tibetic_grammars <- read_excel("Tibetic-Grammars_Review_Master.xlsx")

# Ensure that the total_years_grammar column is numeric
tibetic_grammars <- tibetic_grammars %>%
  mutate(
    is_diss = ifelse(is_diss == "Y", "Dissertation", "Published Grammar"),
    total_years_grammar = as.numeric(total_years_grammar)  # Convert total_years_grammar to numeric if it's not already
  )

# Handling potential NA values or errors in conversion
tibetic_grammars <- tibetic_grammars %>%
  filter(!is.na(total_years_grammar))  # Remove rows where total_years_grammar could not be converted to numeric

# Create the box plot
box_plot <- ggplot(tibetic_grammars, aes(x = is_diss, y = total_years_grammar, fill = is_diss)) +
  geom_boxplot() +
  labs(x = "Type of grammar", y = "Number of years") +
  scale_fill_manual(values = c("Dissertation" = "lightblue", "Published Grammar" = "lightgreen")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",  # Hide the legend as it's redundant
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(size = 12, face = "bold"),  # Only define axis.text.x once
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

# Print the plot
print(box_plot)


# Calculate medians, quartiles, min, and max
statistics <- tibetic_grammars %>%
  group_by(is_diss) %>%
  summarise(
    Minimum = min(total_years_grammar),
    First_Quartile = quantile(total_years_grammar, 0.25),
    Median = median(total_years_grammar),
    Third_Quartile = quantile(total_years_grammar, 0.75),
    Maximum = max(total_years_grammar)
  )

# Print the statistics
print(statistics)







