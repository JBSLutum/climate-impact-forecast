#Probabilistic Crop Model under Uncertainty#

library(dplyr)
library(ggplot2)
library(decisionSupport)
library(tidyverse)

####vorlage####
# Load necessary libraries


# ---- Step 1: Initialize Variables ----
# Assume input variables are pre-loaded as data frames or lists. Placeholders are used here.

# Example input variables (replace with actual data reading logic)
plants_per_hectare <- 40000        # Number of plants per hectare
gram_per_harvested_spear <- 25     # Average weight per spear (grams)
days_photosynthesis <- 100         # Productive photosynthesis days
soil_temperature <- rnorm(100, mean = 21, sd = 3) # Daily soil temperature (simulated example)
daily_precipitation <- runif(100, min = 0, max = 10) # Daily precipitation (mm)

# Risk probabilities and impacts (placeholders)
risks <- data.frame(
  risk = c("drought", "insects", "disease", "wind"),
  probability = c(0.2, 0.15, 0.1, 0.05), # Probabilities of occurrence
  impact = c(0.2, 0.15, 0.1, 0.25) # Impact as fraction reducing potential
)

# ---- Step 2: Calculate Potential from Vegetative Phase ----
calculate_potential <- function(days_photosynthesis, risks) {
  # Base potential
  potential <- ifelse(days_photosynthesis >= 100, 1, days_photosynthesis / 100)
  
  # Apply risks
  for (i in 1:nrow(risks)) {
    if (runif(1) < risks$probability[i]) {
      potential <- potential * (1 - risks$impact[i])
    }
  }
  
  # Simulate fall cooling effect
  cooling_effect <- sample(c(TRUE, FALSE), size = 1, prob = c(0.9, 0.1)) # 90% chance of proper cooling
  if (!cooling_effect) {
    potential <- potential * 0.9
  }
  
  return(potential)
}

potential <- calculate_potential(days_photosynthesis, risks)

# ---- Step 3: Simulate Spear Growth ----
simulate_spear_growth <- function(soil_temperature, potential) {
  # Growth per day based on soil temperature
  daily_growth <- ifelse(soil_temperature > 14 & soil_temperature <= 25,
                         10,
                         ifelse(soil_temperature > 25, 15, 0))
  
  # Quality adjustments for extreme temperatures
  quality <- rep(1, length(daily_growth)) # Start at 100% quality
  quality[soil_temperature > 25] <- 0.9
  quality[soil_temperature < 14] <- 0.8
  
  # Harvest simulation
  spear_lengths <- cumsum(daily_growth)
  harvestable_spears <- spear_lengths[spear_lengths >= 20]
  num_spears <- length(harvestable_spears)
  
  # Adjust for potential and quality
  num_spears <- min(num_spears, round(potential * 7)) # Max spears per plant based on potential
  spear_weights <- gram_per_harvested_spear * quality[1:num_spears]
  
  return(list(
    num_spears = num_spears,
    total_weight = sum(spear_weights, na.rm = TRUE)
  ))
}

growth_results <- simulate_spear_growth(soil_temperature, potential)

# ---- Step 4: Calculate Yield ----
calculate_yield <- function(plants_per_hectare, growth_results) {
  total_spears <- plants_per_hectare * growth_results$num_spears
  total_weight <- plants_per_hectare * growth_results$total_weight
  return(list(
    total_spears = total_spears,
    total_weight_kg = total_weight / 1000 # Convert to kilograms
  ))
}

yield_results <- calculate_yield(plants_per_hectare, growth_results)

# ---- Step 5: Output Results ----
print(paste("Total Yield (kg):", yield_results$total_weight_kg))
print(paste("Total Spears:", yield_results$total_spears))

# Visualization Example
yield_data <- data.frame(
  Day = 1:100,
  SoilTemperature = soil_temperature,
  Precipitation = daily_precipitation
)

ggplot(yield_data, aes(x = Day)) +
  geom_line(aes(y = SoilTemperature, color = "Soil Temperature")) +
  geom_line(aes(y = Precipitation, color = "Precipitation")) +
  labs(title = "Daily Soil Temperature and Precipitation",
       y = "Value", color = "Legend") +
  theme_minimal()

####only yield (perfect condition)####
yield_only<- function(x, varnames){
  # basic variables
  plants_m2 = plants_lfm               
  avg_weight = avg_weight_spear         
  avg_spears = avg_spears_plant 
  potential = 100                 
  quality = 100             
  
  # yield calculation
  yield_m2 = plants_m2 * avg_weight * avg_spears * potential/100 * quality/100
  return(list(yield=yield_m2))
  }
input<- estimate_read_csv("cif_asparagus/crop_model.csv")
crop_model<-mcSimulation(estimate = input,
             outputPath='results',
             model_function = yield_only,
             numberOfModelRuns = 10000,
             functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = crop_model,
                   vars = "yield",
                   method = "boxplot_density",
                   old_names = "yield",
                   new_names = "Outcome distribution for yield per sqm")

####yield + chill probabilities####
yield_only<- function(x, varnames){
  # basic variables
  plants_m2 = plants_lfm               
  avg_weight = avg_weight_spear         
  avg_spears = avg_spears_plant 
  potential = 100                 
  quality = 100
  foil = "black"
  
  # Example daily temperature data for a season (this should be imported from the CSV)
  daily_temperature = c(2, 4, 6, 8, 3, 1, 5, 7, 0, 6, 9, 10)  # Example temperatures for each day
  
  # Call the function to check if the chill requirement is met
  chill_status = calculate_chill(daily_temperature, chill_threshold)
  if(!chill_status){
    potential<-potential-10
    quality<-quality-10
  }
  growth_start <- calculate_growth_start(daily_temperature, growth_threshold, foil)
  if (growth_start>0){
    
  }
  
  # yield calculation
  yield_m2 = plants_m2 * avg_weight * avg_spears * potential/100 * quality/100
  return(list(yield=yield_m2))
}
input<- estimate_read_csv("cif_asparagus/crop_model.csv")
crop_model<-mcSimulation(estimate = input,
                         outputPath='results',
                         model_function = yield_only,
                         numberOfModelRuns = 10000,
                         functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = crop_model,
                   vars = "yield",
                   method = "boxplot_density",
                   old_names = "yield",
                   new_names = "Outcome distribution for yield per sqm")

####chill function####
# Function to check if chill requirement is met based on daily temperature data
calculate_chill <- function(daily_temperature, chill_threshold = 300) {
  
  # Initialize chill accumulation
  chill_hours = 0
  
  # Loop through the daily temperatures to accumulate chill hours
  for (temp in daily_temperature) {
    if (temp >= 0 & temp <= 7) {
      chill_hours = chill_hours + 24  # Add 24 hours (full day) for temperatures in chill range
    }
  }
  
  # Check if chill requirement is fulfilled
  if (chill_hours >= chill_threshold) {
    chill = TRUE
  } else {
    chill = FALSE
  }
  
  return(chill)  # Return the result: TRUE or FALSE
}

####start of growth function####
# Function to calculate when spears start to grow
calculate_growth_start <- function(daily_temperature, growth_threshold, foil) {
  # Find the first day where soil temperature meets or exceeds the growth threshold
  if (foil="black"){growth_threshold<-growth_threshold-7}
  for (day in seq_along(daily_temperature)) {
    if (daily_temperature[day] >= growth_threshold) {
      return(day)    
    }
  }
  return(NA)  # Return NA if growth never starts
}
####make variables####
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("cif_asparagus/crop_model.csv"))
