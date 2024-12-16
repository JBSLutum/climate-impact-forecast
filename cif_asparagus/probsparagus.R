####ProbSparagus####


# vegetative growth function over summer
growth_potential <- function(plants_per_dam_meter, row_spacing, risk_drought, risk_disease, risk_insects, good_photosynthesis_days, required_photosynthesis_days, risk_drought_damage,risk_disease_damage,risk_insects_damage) {
  
  # Calculate plants per hectare
  plants_per_ha <- (10000 / row_spacing) * plants_per_dam_meter  
  # Initial growth potential
  potential <- 100
  
  # Check if photosynthesis requirement is met
  if (good_photosynthesis_days < required_photosynthesis_days) {
    potential <- potential * (1 - good_photosynthesis_days / required_photosynthesis_days))
  }
  
  # Apply risks to potential
  if (runif(1) < risk_drought) {
    potential <- potential - risk_drought_damage  # Example: drought reduces by 15%
  }
  if (runif(1) < risk_disease) {
    potential <- potential - risk_disease_damage  # Example: disease reduces by 10%
  }
  if (runif(1) < risk_insects) {
    potential <- potential - risk_insects_damage   # Example: insects reduce by 5%
  }
  
  # Ensure potential doesn't go below 0 or above 100
  potential <- min(100, max(0, potential))
  
  return(list(potential = potential, plants_per_ha = plants_per_ha))
}

#chill function
chill_accumulation <- function(chill_hours, chill_required, potential, quality, chill_penalty) {
  # Check if chill requirements are met
  chill_met <- chill_hours >= chill_required
  
  
  return(list(chill_met = chill_met)
}


#start of growth function
spear_growth_start <- function(avg_spring_temp, soil_temp_threshold, max_harvest_duration, harvest_end_date) {
  # Estimate the start date based on temperature thresholds
  start_date <- ifelse(avg_spring_temp >= soil_temp_threshold, "early_spring", "late_spring")
  
  # Calculate harvest duration based on the start date
  duration <- ifelse(start_date == "early_spring", max_harvest_duration, max_harvest_duration - 2)
  
  return(list(start_date = start_date, duration = duration, end_date = harvest_end_date))
}


#yield function
harvest_phase <- function(potential, quality, risk_heat, risk_rain, risk_temp_variation, avg_weight_spear, avg_spears_plant, plants_m2) {
  # Adjust quality based on harvest risks
  adjusted_quality <- quality * (1 - risk_heat) * (1 - risk_rain) * (1 - risk_temp_variation)
  
  # Calculate final yield
  yield_m2 <- plants_m2 * avg_weight_spear * avg_spears_plant * (potential / 100) * (adjusted_quality / 100)
  
  return(yield_m2)
}

probsparagus<-function(x, varnames)
{
  list ( potential, plants ) <- growth_potential(plants_per_dam_meter, row_spacing, risk_drought, risk_disease, risk_insects, good_photosynthesis_days, required_photosynthesis_days, risk_drought_damage,risk_disease_damage,risk_insects_damage)
  chill <- chill_accumulation(chill_hours, chill_required)
  
  # Adjust potential and quality if chill is insufficient
  if (!chill) {
    potential <- potential - chill_penalty
    quality <- quality - chill_penalty
  }
  
  start <- spear_growth_start(avg_spring_temp, soil_temp_threshold, max_harvest_duration, harvest_end_date)
  yield <- harvest_phase(potential, quality, risk_heat, risk_rain, risk_temp_variation, avg_weight_spear, avg_spears_plant, plants_m2)
    
  
}
