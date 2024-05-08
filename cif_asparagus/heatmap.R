# Heat map of measures and outcomes

# 7 columns and 19 rows

certification_measures_impact_data <- certification_measures_simulation$y

library(tidyr)

pivoted_outcome_data <- tidyr::pivot_longer(certification_measures_impact_data, cols = names(certification_measures_impact_data), 
                                            names_to = "Outcome with measure", values_to = "Relative difference")

library(ggplot2)

# pivot the data and split names ####
library(stringr)
pivoted_outcome_data["Outcome"] <-  str_extract(pivoted_outcome_data$`Outcome with measure`, 
                                                c("yield", "cost", "risk_increase", 
                                                  "risk_reduction", "adaptation", 
                                                  "mitigation"))

pivoted_outcome_data["Measure"] <-  str_remove(pivoted_outcome_data$`Outcome with measure`, 
                                               c("_yield", "_cost", "_risk_increase", 
                                                 "_risk_reduction", "_adaptation", 
                                                 "_mitigation"))

# create labels for 'high', 'medium' and 'low' impact ####

# make some data with mean differences ####

summarized_pivoted_outcome_data <- pivoted_outcome_data %>% 
  group_by(Measure, Outcome) %>% 
  dplyr::summarize(median = median(`Relative difference`), 
                   mean = mean(`Relative difference`))  


# with color for direction (positive, negative)

summarized_pivoted_outcome_data$effect_direction <- ifelse(summarized_pivoted_outcome_data$median > 0, "positive", 
                                                           ifelse(summarized_pivoted_outcome_data$median < 0, "negative", 
                                                                  "no_effect"))

summarized_pivoted_outcome_data$effect_strength <- ifelse(summarized_pivoted_outcome_data$median > 0.2 | 
                                                            summarized_pivoted_outcome_data$median < -0.2, "H", 
                                                          ifelse(summarized_pivoted_outcome_data$median > 0.05  & 
                                                                   summarized_pivoted_outcome_data$median <= 0.2 | 
                                                                   summarized_pivoted_outcome_data$median < -0.05 & 
                                                                   summarized_pivoted_outcome_data$median >= -0.2, "M", 
                                                                 ifelse(summarized_pivoted_outcome_data$median == 0, "", "L")))

#Rename columns with variables ####

summarized_pivoted_outcome_data$Outcome <- factor(summarized_pivoted_outcome_data$Outcome, 
                                                  levels = c("adaptation", "mitigation", 
                                                             "yield", "cost", 
                                                             "risk_reduction", "risk_increase"), 
                                                  labels = c("Climate Change\nAdaptation", 
                                                             "Climate Change\nMitigation",
                                                             "Expected change\nin yield", 
                                                             "Implementation\ncost",
                                                             "Risks reduced\nby measure",
                                                             "Risks increased\nby measure"))

summarized_pivoted_outcome_data$effect_direction <- factor(summarized_pivoted_outcome_data$effect_direction, 
                                                           labels = c("Negative", "None", "Positive"))


summarized_pivoted_outcome_data$Measure <- factor(summarized_pivoted_outcome_data$Measure, 
                                                  levels = c("energy_use_plan" , 
                                                             "energy_equipment", 
                                                             "renewable_energy", 
                                                             "waste_water", 
                                                             "water_reservoir", 
                                                             "a_evap_trans_spray", 
                                                             "irrigation_methods", 
                                                             "irrigation_scheduling", 
                                                             "drainage", 
                                                             "composting", 
                                                             "nutrient_mgmt", 
                                                             "ipm_practice", 
                                                             "reincorporation", 
                                                             "cover_crop", 
                                                             "buffer", 
                                                             "conversion", 
                                                             "recycling", 
                                                             "waste_plan", 
                                                             "plastic_reduction", 
                                                             "plastic_reuse"),
                                                  labels = c("Energy use plan",
                                                             "Energy equipment",
                                                             "Renewable energy",
                                                             "Waste water", 
                                                             "Water reservoir", 
                                                             "Anti-evapotranspiration spray", 
                                                             "Irrigation methods", 
                                                             "Irrigation scheduling", 
                                                             "Drainage management", 
                                                             "Composting", 
                                                             "Nutrient management", 
                                                             "Integrated Pest Management", 
                                                             "Reincorporate crop residues", 
                                                             "Cover crops", 
                                                             "Buffer zone", 
                                                             "Conversion of unproductive land", 
                                                             "Recycling plastic", 
                                                             "Waste plan", 
                                                             "Plastic reduction",
                                                             "Plastic re-use"))



# make the plot with names ####
ggplot(data = summarized_pivoted_outcome_data, aes(x = Outcome,
                                                   y = Measure, 
                                                   fill = effect_direction, 
                                                   label = effect_strength)) + 
  geom_tile(width = 0.9, height = 0.9) + 
  geom_text() + 
  scale_fill_manual(values = c("firebrick", "grey80", "cadetblue")) + #neg., none, positive
  labs(x = "Expected outcome", y= "Certification measure", fill = "Effect") + 
  scale_y_discrete(limits = rev(levels(summarized_pivoted_outcome_data$Measure))) +
  theme_minimal() + 
  theme(panel.grid = element_blank())

# make the plot with numbers ####
ggplot(data = summarized_pivoted_outcome_data, aes(x = Outcome, y = Measure, fill = median)) + 
  geom_tile() + 
  geom_text(aes(x = Outcome, y = Measure, label = round(mean, 1), fill = NULL), data = summarized_pivoted_outcome_data) +
  scale_fill_gradient2(low = "red4", mid = "white", midpoint = 0, high = "blue4")
