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



#####sections#####
phase_1 <- function(p1_risk_drought, p1_risk_drought_damage, 
                        p1_risk_insects, p1_risk_insects_damage,
                        p1_risk_disease, p1_risk_disease_damage,
                        p1_risk_wind, p1_risk_wind_damage,
                        growth_potential=100, min_par_days, par_days) {
  
  if (par_days<min_par_days){
    growth_potential<- growth_potential - (min_par_days - par_days)
  }
  
  # Check for risks and apply damages
  if (p1_risk_drought) {
    growth_potential <- growth_potential - p1_risk_drought_damage
  }
  if (p1_risk_insects) {
    growth_potential <- growth_potential - p1_risk_insects_damage
  }
  if (p1_risk_disease) {
    growth_potential <- growth_potential - p1_risk_disease_damage
  }
  if (p1_risk_wind) {
    growth_potential <- growth_potential - p1_risk_wind_damage
  }
  
  # Ensure growth potential stays within bounds
  growth_potential <- max(0, min(100, growth_potential))
  
  return(growth_potential)
}

phase_2 <- function(chill_hours, required_chill_hours) {
  # Calculate chill requirement as a percentage
  chill_percentage <- (chill_hours / required_chill_hours) * 100
  chill_percentage <- min(100, max(0, chill_percentage)) # Bound to 0-100%
  
  return(chill_percentage)
}

phase_3 <- function(harvest_start_day, harvest_end_day) {
  # Calculate the length of the harvest season
  harvest_length <- harvest_end_day - harvest_start_day
  
  # Ensure the harvest length is positive and within a reasonable range
  harvest_length <- max(0, harvest_length)
  
  return(harvest_length)
}

phase_4 <- function(growth_potential, chill_percentage, harvest_length, base_season_length,
                        p4_risk_heat, p4_risk_heat_damage,
                        p4_risk_rain, p4_risk_rain_damage, 
                        p4_risk_temperature_variation, p4_risk_temperature_variation_damage,
                        base_yield, base_quality) {
  
  total_yield<-0
  actual_yield<-0
  overall_quality<-0
  
  # Adjust growth potential based on chill percentage
  growth_potential <- growth_potential * (chill_percentage / 100)
  quality_potential <- base_quality * (chill_percentage / 100)
  
  yield_potential <- base_yield * (harvest_length/base_season_length)
  daily_yield <- yield_potential/harvest_length
  
  for (i in length(harvest_length)){
  
  # Check for harvest risks and apply damages to growth potential and quality
  if (p4_risk_heat) {
    growth_potential <- growth_potential - p4_risk_heat_damage
    quality_potential <- quality_potential - (p4_risk_heat_damage * 2) # Assume heat impacts quality as well
  }
  else if (p4_risk_rain) {
    growth_potential <- growth_potential - p4_risk_rain_damage
    quality_potential <- quality_potential - (p4_risk_rain_damage * 2) # Rain impacts quality too
  }
  else if (p4_risk_temperature_variation) {
    growth_potential <- growth_potential - p4_risk_temperature_variation_damage
    quality_potential <- quality_potential - (p4_risk_temperature_variation_damage * 2) # Variation impacts quality
  }
  
  # Ensure growth potential and quality stay within bounds
  growth_potential <- max(0, min(100, growth_potential))
  quality_potential <- max(0, min(100, quality_potential))
  
  # Adjust daily yield by the daily growth potential
  yield_per_day <- daily_yield * (growth_potential / 100)
  
  # daily quality adjustment
  daily_quality <- base_quality * (quality_potential / 100)
  
  #sum
  total_yield<-total_yield+yield_per_day
  if (daily_quality>75){
    actual_yield<-actual_yield+yield_per_day
  }
  overall_quality<-overall_quality+daily_quality
  }
  overall_quality<-overall_quality/harvest_length
  
  
  return(list(yield_per_ha = actual_yield, total_yield = total_yield, quality = overall_quality))
}

prob1<-function(x, varnames){
  growth_potential<-phase_1(p1_risk_drought, p1_risk_drought_damage, 
                                        p1_risk_insects, p1_risk_insects_damage,
                                        p1_risk_disease, p1_risk_disease_damage,
                                        p1_risk_wind, p1_risk_wind_damage,
                                        growth_potential=100, min_par_days, par_days)
  
  chill_percentage<-phase_2(chill_hours, required_chill_hours)
  
  harvest_length<-phase_3(harvest_start_day, harvest_end_day)
  
  yield <- phase_4(growth_potential, chill_percentage, harvest_length, base_season_length,
                      p4_risk_heat, p4_risk_heat_damage,
                      p4_risk_rain, p4_risk_rain_damage, 
                      p4_risk_temperature_variation, p4_risk_temperature_variation_damage,
                      base_yield, base_quality)
  
  return(list(actual_yield=yield[1],total_yield=yield[2],quality=yield[3]))
}

library(decisionSupport)
input<- estimate_read_csv("cif_asparagus/probsparagus.csv")
crop_model<-mcSimulation(estimate = input,
                         outputPath='results_prob',
                         model_function = prob1,
                         numberOfModelRuns = 10000,
                         functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = crop_model,
                   vars = "actual_yield",
                   method = "boxplot_density",
                   old_names = "actual_yield",
                   new_names = "Outcome distribution for yield per ha")

####make variables####
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("cif_asparagus/probsparagus.csv"))

