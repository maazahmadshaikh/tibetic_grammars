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
grammars_master <- read_excel("Tibetic-Grammars_Master-Database.xlsx")

head(grammars_master)
table(grammars_master$labels)


#caluclate the total number of grammars
total_grammars <- nrow(grammars_master)
table(total_grammars)



# Define the desired order for branches
branch_order <- c("NW", "W", "C", "SW", "S", "SE", "E", "NE")
