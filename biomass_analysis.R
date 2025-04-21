# Health Impacts of Indoor Air Pollution from Biomass Fuel Use — Analysis by Jackline Lagat

# Loading the required libraries for my analysis
library(dplyr)
library(ggplot2)

# Loading my research dataset
# I collected this data during my study in Soweto, Nairobi County
biomass_data <- read.csv("data/biomass_soweto_data.csv")

# Quick preview of my data to confirm it's loaded correctly
head(biomass_data)

# Data Cleaning and Recoding
# I recoded the 'Fuel' variable to group biomass fuels (Charcoal, Wood, Saw dust) together
biomass_data$Fuel <- ifelse(biomass_data$Fuel %in% c("Charcoal", "Wood", "Saw dust"), "Biomass", "Non-Biomass")
biomass_data$Fuel <- factor(biomass_data$Fuel)

# Recoding ventilation type: Natural = 0, Artificial = 1
biomass_data$Ventilation <- ifelse(biomass_data$Ventilation == "Natural", 0, 1)

# Recoding smoking status: Yes = 1, No = 0
biomass_data$Smoking <- ifelse(biomass_data$Smoking == "Yes", 1, 0)

# Verifying the structure of the data after cleaning
str(biomass_data)

# Visualizing the frequency of different diseases reported by participants
# This helps me see which health effects were most common
ggplot(biomass_data, aes(x = Disease)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Reported Health Effects from Biomass Fuel Use",
       x = "Disease Type",
       y = "Number of Cases") +
  theme_minimal()

# Chi-square test to check if there's an association between type of fuel used and disease occurrence
# This helps me see if fuel type has a significant relationship with the diseases reported
table_fuel_disease <- table(biomass_data$Fuel, biomass_data$Disease)
chisq.test(table_fuel_disease)

# Running a Logistic Regression Model
# Here I'm checking how Fuel type, Ventilation, and Smoking influence disease occurrence
# (Assuming 'Disease' is a binary variable: 1 = has disease, 0 = no disease)
model <- glm(Disease ~ Fuel + Ventilation + Smoking, family = binomial, data = biomass_data)

# Reviewing the model summary to interpret the results
summary(model)

# End of my analysis for this study
