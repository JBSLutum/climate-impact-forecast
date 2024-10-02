library(tidyverse)
library(decisionSupport)

vv#simple base model####
baseline <- function(x, varnames)
{
  
  quan_100<-vv(yield, var_CV, n)
  qual_100<-vv(quality, var_CV, n)
  season_length<-vv(season_length, var_CV, n)
  RCP<-c(0, 2.5, 4.5, 8.5)

  for (i in RCP){

    yield<-quan_100
    quality<-qual_100
    season_days<-season_length
    foil<-0
    n<-0
    if(i==0){
      #weather base scenarios year effect
      chill_requirement<-chance_event(chill_req_0)
      warm_spring<-chance_event(warm_spring_0)
      late_frost<-chance_event(late_frost_0)
      daily_temp_change<-chance_event(daily_temp_change_0, n=120)
    }
    if(i==2.5){
      #weather scenarios RCP2.5 year effect
      chill_requirement<-chance_event(chill_req_2.5)
      warm_spring<-chance_event(warm_spring_2.5)
      late_frost<-chance_event(late_frost_2.5)
      daily_temp_change<-chance_event(daily_temp_change_2.5, n=120)
    }
    if(i==4.5){
      #weather scenarios RCP4.5 year effect
      chill_requirement<-chance_event(chill_req_4.5)
      warm_spring<-chance_event(warm_spring_4.5)
      late_frost<-chance_event(late_frost_4.5)
      daily_temp_change<-chance_event(daily_temp_change_4.5, n=120)
    }
    if(i==8.5){
      #weather scenarios RCP8.5 year effect
      chill_requirement<-chance_event(chill_req_8.5)
      warm_spring<-chance_event(warm_spring_8.5)
      late_frost<-chance_event(late_frost_8.5)
      daily_temp_change<-chance_event(daily_temp_change_8.5, n=120)
    }
    #check if chill requirement is reached
    if(!chill_requirement)
      {yield<-yield*0.99
      quality<-quality*0.99}
    #check spring temperature conditions
    if(warm_spring)
    {if(season_days<=100){
        season_days<-season_days+20
    }
      else{
        season_days<-season_length
      }
        
        }
    #check late frost occurrence
    if(late_frost)
      {yield<-yield*0.99
      quality<-quality*0.99}
    
    season_days<-ceiling(season_days)
    for(j in 2:season_days){
      if(!is.na(daily_temp_change[j]) && !is.na(daily_temp_change[j-1])) {
      if(daily_temp_change[j]==daily_temp_change[j-1])
        {n<-n+1}
      else
      {if(n<2)
      {yield<-yield*0.95 
          quality<-quality*0.95}
      n<-0}
      if(n==1)
      {foil<-foil+1}
      }
      if(n>=2)
      {yield<-yield*1.2 
      quality<-quality*1.2}
     else {
      
    }
    
  }
    if(i==0){
      yield_baseline<-yield
      quality_baseline<-quality
      work_baseline<-foil
    }
    if(i==2.5){
      yield_2.5<-yield
      quality_2.5<-quality
      work_2.5<-foil
    }
    if(i==4.5){
      yield_4.5<-yield
      quality_4.5<-quality
      work_4.5<-foil
    }
    if(i==8.5){
      yield_8.5<-yield
      quality_8.5<-quality
      work_8.5<-foil
    }
    
  }
  #utility2.5<-(yield_baseline*quality_baseline-work_baseline)-(yield_2.5*quality_2.5-work_2.5)
  #utility4.5<-(yield_baseline*quality_baseline-work_baseline)-(yield_4.5*quality_4.5-work_4.5)
  #utility8.5<-(yield_baseline*quality_baseline-work_baseline)-(yield_8.5*quality_8.5-work_8.5)
  # utility2.5<-(yield_baseline*quality_baseline)-(yield_2.5*quality_2.5)
  # utility4.5<-(yield_baseline*quality_baseline)-(yield_4.5*quality_4.5)
  # utility8.5<-(yield_baseline*quality_baseline)-(yield_8.5*quality_8.5)
  utility2.5<-(yield_2.5+quality_2.5)
  utility4.5<-(yield_4.5+quality_4.5)
  utility8.5<-(yield_8.5+quality_8.5)
  return(list(utility2.5=utility2.5,
              utility4.5=utility4.5,
              utility8.5=utility8.5
              ))
}

input<-read.csv("input_m1_poster.csv")
decisionSupport("input_m1_poster.csv",
                outputPath='results_m1',
                welfareFunction=baseline,
                numberOfModelRuns=1000,
                functionSyntax="plainNames")

input$lower<-as.numeric(input$lower)
t1<-mcSimulation(estimate = input,
             outputPath='results_m12',
            model_function = baseline,
            numberOfModelRuns = 800,
            functionSyntax = "plainNames")

t1
plot_distributions(mcSimulation_object = t1,
                   vars = "utility2.5",
                   method = "boxplot_density",
                   old_names = "utility2.5",
                   new_names = "Outcome distribution for utility function")

##figure tries####
mc<-read.csv("results_m1/mcSimulationResults.csv")
legend_table<-read.csv("input.csv")
mc_EVPI<-mc[,-grep("output",colnames(mc))]
dir.create("Figures")
empirical_EVPI(mc_EVPI,"output_2",write_table=TRUE,fileformat="png",outfolder="Figures",
               p_spearman=0.05, legend_table=read.csv("input.csv"),#legend_table,
               output_legend_table=read.csv("input.csv"))#legend_table)


make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("input_m1.csv"))



#extended baseline model####
baseline_potential <- function(x, varnames)
{
  quan_100<-vv(yield, var_CV, n)
  qual_100<-vv(quality, var_CV, n)
  season_length<-vv(season_length, var_CV, n)
  RCP<-c(0, 2.5, 4.5, 8.5)
  
  for (i in RCP){
    
    yield<-quan_100
    quality<-qual_100
    season_days<-season_length
    foil<-0
    n<-0
    if(i==0){
      weather_data_baseline <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "baseline")
      weather_data<-weather_data_baseline
      chill_requirement<-chance_event(chill_requirement)
    }
    if(i==2.5){
      weather_data_rcp2.5 <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "rcp2.5")
      weather_data<-weather_data_rcp2.5
      chill_requirement<-chance_event(chill_requirement)
    }
    if(i==4.5){
      weather_data_rcp4.5 <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "rcp4.5")
      weather_data<-weather_data_rcp4.5
      chill_requirement<-chance_event(chill_requirement)
    }
    if(i==8.5){
      weather_data_rcp8.5 <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "rcp8.5")
      weather_data<-weather_data_rcp8.5
      chill_requirement<-chance_event(chill_requirement)
    }
    #check if chill requirement is reached
    if(!chill_requirement)
    {yield<-yield-5
    quality<-quality-5}
    
    bodentemperatur <- 0
    folie <- "none"  # Optionen: "none", "normal", "thermo"
    optimal_temperature <- 21
    arbeitseinsatz_counter <- 0
    ernte_möglich <- FALSE
    temperatursumme <- 0
    
    #check spring temperature conditions
    for (i in 1:nrow(weather_data)) {
      # Extrahiere die Wetterdaten für den aktuellen Tag
      current_temp <- weather_data$temperature[i]
      frost <- weather_data$frost[i]
      
      # Bodentemperatur berechnen (2 Grad unter der Lufttemperatur)
      bodentemperatur <- current_temp - 2
      
      
      
      # Folie zur Temperaturanpassung
      if (frost) {
        if (folie != "thermo") {
          folie <- "thermo"
          bodentemperatur <- bodentemperatur + 8
          arbeitseinsatz_counter <- arbeitseinsatz_counter + 1
          quality<-quality-1
        }
      } else {
        if (bodentemperatur < optimal_temperature - 8 && folie != "normal") {
          folie <- "normal"
          bodentemperatur <- bodentemperatur + 8
          arbeitseinsatz_counter <- arbeitseinsatz_counter + 1
        } else if (bodentemperatur > optimal_temperature && folie != "none") {
          folie <- "none"
          bodentemperatur <- bodentemperatur - 8
          arbeitseinsatz_counter <- arbeitseinsatz_counter + 1
        }
      }
      
      # Überprüfen, ob die Bodentemperatur das Spargelwachstum unterstützt
      if (bodentemperatur >= 12 && bodentemperatur <= 21) {
        temperatursumme <- temperatursumme + bodentemperatur
      }
      
      # Überprüfen, ob die Temperatur-Summe für die Ernte erreicht wurde
      if (temperatursumme >= vv(yield, var_CV, n)) {
        ernte_möglich <- TRUE
        harvest_start<-i
        break
      }
    }
    harvest_days<-175-harvest_start
    # Simulation der Ernte
    for (i in 1:harvest_days) {
      if (i > nrow(weather_data)) break
      current_temp <- weather_data$temperature[i]
      frost <- weather_data$frost[i]
      storm <- weather_data$storm[i]
      bodentemperatur <- current_temp - 2
      
      if (bodentemperatur < optimal_temperature - 8 && folie != "normal") {
        folie <- "normal"
        bodentemperatur <- bodentemperatur + 8
        arbeitseinsatz_counter <- arbeitseinsatz_counter + 1
      } else if (bodentemperatur > optimal_temperature && folie != "none") {
        folie <- "none"
        bodentemperatur <- bodentemperatur - 8
        arbeitseinsatz_counter <- arbeitseinsatz_counter + 1
      }
      
      # Check if conditions are not optimal
      if (bodentemperatur < 12 || bodentemperatur > 21 || storm) {
        bad_day <- bad_day + 1
        arbeitseinsatz_counter <- arbeitseinsatz_counter + 1  # Extra work due to storm
      }
      
      # Update harvest days counter
      day_count <- day_count + 1
      
      # Break the loop if all harvest days are completed
      if (day_count >= harvest_days) {
        break
      }
    }
    
    # Calculate the percentage of harvest yield
    if (day_count > 0) {
      yield <- yield - (bad_day / day_count * 100)
    }
    }
    if(i==0){
      yield_baseline<-yield
      quality_baseline<-quality
      work_baseline<-foil
    }
    if(i==2.5){
      yield_2.5<-yield
      quality_2.5<-quality
      work_2.5<-foil
    }
    if(i==4.5){
      yield_4.5<-yield
      quality_4.5<-quality
      work_4.5<-foil
    }
    if(i==8.5){
      yield_8.5<-yield
      quality_8.5<-quality
      work_8.5<-foil
    }
  
  return(list(yield_baseline, quality_baseline, work_baseline,
              yield_2.5, quality_2.5, work_2.5,
              yield_4.5, quality_4.5, work_4.5,
              yield_8.5, quality_8.5, work_8.5
  ))
}

decisionSupport("input2.csv",
                outputPath='results_m2',
                welfareFunction=baseline,
                numberOfModelRuns=10000,
                functionSyntax="plainNames")


make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("input.csv"))

#bigger model####
cif_aspa<-function(x, varnames)
{
  RCP<-c(0, 2.5, 4.5, 8.5)
  #RCP<-0
  for (i in RCP){
    ##wetter daten erstellen####
    if(i==0){
      weather_data_baseline <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "baseline")
      weather_data<-weather_data_baseline
      }
    if(i==2.5){
      weather_data_rcp2.5 <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "rcp2.5")
      weather_data<-weather_data_rcp2.5
      }
    if(i==4.5){
      weather_data_rcp4.5 <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "rcp4.5")
      weather_data<-weather_data_rcp4.5
      }
    if(i==8.5){
      weather_data_rcp8.5 <- weather_simulation(days = 365, rain_prob = chance_event(rain_prob), strong_rain_prob = chance_event(strong_rain_prob), storm_prob = chance_event(storm_prob), frost_prob = chance_event(frost_prob), rcp_scenario = "rcp8.5")
      weather_data<-weather_data_rcp8.5
      }
  ##spring dams####
    #Bodenbefahrbarkeit testen
    warmday<-0
    rainday<-0
    fieldday<-0
    for (i in 1:365){
      if(weather_data$temperature[i]>=5){warmday<-warmday+1}
      if(weather_data$rain[i]){rainday<-rainday+1}
      if(rainday>2){
        warmday<-warmday-2
        rainday<-rainday-1
      }
      fieldday<-fieldday+1
      if (warmday>=10 && sum(weather_data$rain[(fieldday-10):fieldday])>=1){
        break}
    }
  ##speargrowth####
  ##harvest####
  ##ferngrowth####
  ##winter dams####
}

#ganz anderer ansatz (quatsch)####
# Laden notwendiger Pakete
library(dplyr)
library(ggplot2)

# Definition der Parameter
set.seed(123)

# Wetterbedingungen über ein Jahr simulieren
simulate_weather <- function() {
  weather_events <- c("Regen", "Frost", "Hitze", "Starkregen", "Trockenheit", "Sturm")
  weather <- sample(weather_events, size = 365, replace = TRUE)
  return(weather)
}

# Pflanzenbauliche Abläufe und ihre Abhängigkeit von Wetterbedingungen
processes <- data.frame(
  process = c("Aufbauen Ernte Damm", "Kältebedarf", "Stangen Wuchs", "Ernte", "Kraut Wuchs", "Erstellen Winter Damm"),
  start_day = c(1, 90, 120, 150, 180, 300),
  end_day = c(30, 119, 149, 179, 299, 365)
)

# Maßnahmen des Bauern und ihre Effekte
actions <- data.frame(
  action = c("Bewässerung", "Folie", "Düngung", "Pflanzenschutz"),
  effect = c("Erhöht Erntemenge", "Verbessert Qualität", "Erhöht Erntemenge", "Verbessert Qualität")
)

# Output-Parameter initialisieren
output <- data.frame(
  day = 1:365,
  erntemenge = rep(0, 365),
  qualität = rep(0, 365),
  arbeitsaufwand = rep(0, 365)
)

# Simulation des Erntejahres
weather <- simulate_weather()

for (day in 1:365) {
  for (i in 1:nrow(processes)) {
    if (day >= processes$start_day[i] && day <= processes$end_day[i]) {
      if (weather[day] == "Regen" && processes$process[i] == "Aufbauen Ernte Damm") {
        output$erntemenge[day] <- output$erntemenge[day] + 1
        output$arbeitsaufwand[day] <- output$arbeitsaufwand[day] + 2
      } else if (weather[day] == "Frost" && processes$process[i] == "Kältebedarf") {
        output$qualität[day] <- output$qualität[day] + 1
      } else if (weather[day] == "Hitze" && processes$process[i] == "Stangen Wuchs") {
        output$erntemenge[day] <- output$erntemenge[day] - 1
      }
      # Weitere Bedingungen hinzufügen
    }
  }
}

# Maßnahmen des Bauern hinzufügen (z.B. Bewässerung erhöht Erntemenge)
output <- output %>%
  mutate(
    erntemenge = erntemenge + 10,  # Beispielhafter Effekt der Bewässerung
    qualität = qualität + 5,       # Beispielhafter Effekt der Düngung
    arbeitsaufwand = arbeitsaufwand + 3
  )

# Ergebnis anzeigen
print(output)

# Plotten der Ergebnisse
ggplot(output, aes(x = day)) +
  geom_line(aes(y = erntemenge, color = "Erntemenge")) +
  geom_line(aes(y = qualität, color = "Qualität")) +
  geom_line(aes(y = arbeitsaufwand, color = "Arbeitsaufwand")) +
  labs(title = "Simulation des Spargeljahres", x = "Tag", y = "Wert") +
  theme_minimal()

#wetter versuch####
weather_simulation <- function(
    days = 365, 
    rain_prob = 0.2, 
    strong_rain_prob = 0.05, 
    storm_prob = 0.02, 
    frost_prob = 0.1,
    rcp_scenario = "baseline" # Options: "baseline", "rcp2.5", "rcp4.5", "rcp8.5"
) {
  # Create a dataframe for weather data
  weather_data <- data.frame(
    day = 1:days,
    air_temperature = numeric(days),
    soil_temperature = numeric(days),
    rain = logical(days),
    strong_rain = logical(days),
    storm = logical(days),
    frost = logical(days)
  )
  
  # Define temperature adjustments for each RCP scenario
  rcp_adjustments <- list(
    baseline = 0,
    rcp2.5 = 1.0,  # Average increase of 1.0°C
    rcp4.5 = 2.0,  # Average increase of 2.0°C
    rcp8.5 = 4.0   # Average increase of 4.0°C
  )
  
  # Define the seasonal air temperature function with RCP adjustments
  average_air_temperature <- function(day, rcp) {
    base_temp <- 10 + 10 * sin(2 * pi * (day / days - 0.25)) # Average air temperature 10°C with a variation of ±10°C
    return(base_temp + rcp_adjustments[[rcp]])
  }
  
  # Function to estimate soil temperature based on air temperature
  estimate_soil_temperature <- function(air_temp) {
    soil_temp <- air_temp - 2 # Soil temperature is roughly 2°C lower than air temperature
    if (soil_temp < 0) soil_temp <- 0
    return(soil_temp)
  }
  
  # Simulate weather data
  for (i in 1:days) {
    # Simulate air temperature with seasonal variation and random noise
    weather_data$air_temperature[i] <- average_air_temperature(i, rcp_scenario) + rnorm(1, 0, 2)
    # Estimate soil temperature based on air temperature
    weather_data$soil_temperature[i] <- estimate_soil_temperature(weather_data$air_temperature[i])
    # Simulate rain based on given probability
    weather_data$rain[i] <- runif(1) < rain_prob
    # Simulate strong rain based on given probability
    weather_data$strong_rain[i] <- runif(1) < strong_rain_prob
    # Simulate storm based on given probability
    weather_data$storm[i] <- runif(1) < storm_prob
    # Simulate frost based on given probability and temperature condition
    weather_data$frost[i] <- weather_data$soil_temperature[i] <= 0 && runif(1) < frost_prob
  }
  
  return(weather_data)
}
}



####Poster####

poster <- function(){
  mittel_winter_temp<-vv(winter, var_CV, n)  # Durchschnittliche Wintertemperatur (°C)

  mittel_fruhling_temp<-vv(spring, var_CV, n)  # Durchschnittliche Frühlingstemperatur (°C)

  mittel_wetterwechsel<-vv(temp_change, var_CV, n)   # Durchschnittliche Anzahl Wetterwechsel

  prob_spater_frost<-vv(frost, var_CV, n)     # Wahrscheinlichkeit für späten Frost
  
  mittel_extremwetter<-vv(extrem, var_CV, n)   # Durchschnittliche Anzahl Extremwetterereignisse

  mittel_hitzewellen<-vv(heat, var_CV, n)    # Durchschnittliche Anzahl Hitzewellen

 jahr_kosten<-0
  
  
  scenarios<-c("SSP1_26","SSP2_45", "SSP3_70", "SSP5_85" )
  for (i in scenarios){
    if (i==SSP1_26){
      winter_temp<-mittel_winter_temp+1  # Durchschnittliche Wintertemperatur (°C)
      fruhling_temp<-mittel_fruhling_temp+1  # Durchschnittliche Frühlingstemperatur (°C)
      wetterwechsel<-mittel_wetterwechsel+0   # Durchschnittliche Anzahl Wetterwechsel
      spater_frost<-chance_event(prob_spater_frost-0.05)     # Wahrscheinlichkeit für späten Frost
      extremwetter<-mittel_extremwetter+0   # Durchschnittliche Anzahl Extremwetterereignisse
      hitzewellen<-mittel_hitzewellen+0.5    # Durchschnittliche Anzahl Hitzewellen
      
    } else if (scenario == "SSP2_45") {
      winter_temp <- mittel_winter_temp + 2  # Durchschnittliche Wintertemperatur (°C)
      fruhling_temp <- mittel_fruhling_temp + 2  # Durchschnittliche Frühlingstemperatur (°C)
      wetterwechsel <- mittel_wetterwechsel + 1   # Durchschnittliche Anzahl Wetterwechsel
      spater_frost <- chance_event(prob_spater_frost)     # Wahrscheinlichkeit für späten Frost
      extremwetter <- mittel_extremwetter + 1   # Durchschnittliche Anzahl Extremwetterereignisse
      hitzewellen <- mittel_hitzewellen + 1    # Durchschnittliche Anzahl Hitzewellen
      
    } else if (scenario == "SSP3_70") {
      winter_temp <- mittel_winter_temp + 3  # Durchschnittliche Wintertemperatur (°C)
      fruhling_temp <- mittel_fruhling_temp + 3  # Durchschnittliche Frühlingstemperatur (°C)
      wetterwechsel <- mittel_wetterwechsel + 2   # Durchschnittliche Anzahl Wetterwechsel
      spater_frost <- chance_event(prob_spater_frost + 0.05 )    # Wahrscheinlichkeit für späten Frost
      extremwetter <- mittel_extremwetter + 2   # Durchschnittliche Anzahl Extremwetterereignisse
      hitzewellen <- mittel_hitzewellen + 2    # Durchschnittliche Anzahl Hitzewellen
      
    } else if (scenario == "SSP5_85") {
      winter_temp <- mittel_winter_temp + 4  # Durchschnittliche Wintertemperatur (°C)
      fruhling_temp <- mittel_fruhling_temp + 4  # Durchschnittliche Frühlingstemperatur (°C)
      wetterwechsel <- mittel_wetterwechsel + 3   # Durchschnittliche Anzahl Wetterwechsel
      spater_frost <- chance_event(prob_spater_frost - 0.1)    # Wahrscheinlichkeit für späten Frost
      extremwetter <- mittel_extremwetter + 3   # Durchschnittliche Anzahl Extremwetterereignisse
      hitzewellen <- mittel_hitzewellen + 3    # Durchschnittliche Anzahl Hitzewellen
    }
    
    
    # Kältebedarf im Winter
    if (winter_temp > -5) {
      jahr_kosten <- jahr_kosten + vv(kosten_kältebedarf_nicht_gedeckt, var_CV, n)
    }
    
    # Warmer Frühling
    if (fruhling_temp > 10) {
      jahr_kosten <- jahr_kosten + vv(kosten_frühlings_temperatur, var_CV, n)
    }
    
    # Wetterwechsel
    jahr_kosten <- jahr_kosten + wetterwechsel * vv(kosten_wetterwechsel, var_CV, n)
    
    # Später Frost
    if (spaeter_frost == 1) {
      jahr_kosten <- jahr_kosten + vv(kosten_spaeter_frost, var_CV, n)
    }
    
    # Extremwetterereignisse
    jahr_kosten <- jahr_kosten + extremwetter * vv(kosten_extremwetter, var_CV, n)
    
    # Hitzewellen
    jahr_kosten <- jahr_kosten + hitzewellen * vv(kosten_hitzewellen, var_CV, n)
    
    if (i==SSP1_26){
      kosten1_26<-jahr_kosten     
    } else if (scenario == "SSP2_45") {
      kosten2_45<-jahr_kosten 
    } else if (scenario == "SSP3_70") {
      kosten3_70<-jahr_kosten    
    } else if (scenario == "SSP5_85") {
      kosten5_85<-jahr_kosten 
   }
  }
  return(c(kosten1_26<-kosten1_26,
           kosten2_45<-kosten2_45,
           kosten3_70<-kosten3_70,
           kosten5_85<-kosten5_85))
  }

library(ggplot2)

test <- data.frame("SSP5_8.5" = rnorm(n = 10000, mean = 6.73, sd = 2.3), 
                   "SSP3_7.0" = rnorm(n = 10000, mean = 5.45, sd = 2), 
                   "SSP2_4.5" = rnorm(n = 10000, mean = 2.56, sd = 1.5),
                   "SSP1_2.6" = rnorm(n = 10000, mean = 1.9, sd = 1.2))

stacked_test <- stack(test)

postergrafik<-ggplot(stacked_test, 
       aes(x=values,group=ind,fill=ind )) +
  geom_density(colour=NA,alpha=.5) +
  ylab("Probability density") +
  xlab("Kostensteigerung in %")+
  labs(fill="Klimaszenario")+
  theme(text=element_text(size=20))
postergrafik

ggsave("postergrafik.png",postergrafik,device = "png")
ggsave()