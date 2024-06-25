library(readxl)
library(ggplot2)
library(dplyr)
library(mosaic)
library(rvest)
library(chillR)
library(bayesplot)
library(ggridges)
library(ggpubr)
#model1####
model_baseline<-read.csv("results_m1/mcSimulationResults.csv")

names(model_baseline)[names(model_baseline) == 'output_1'] <- 'yield_baseline'
names(model_baseline)[names(model_baseline) == 'output_2'] <- 'quality_baseline'
names(model_baseline)[names(model_baseline) == 'output_3'] <- 'work_baseline'
names(model_baseline)[names(model_baseline) == 'output_4'] <- 'yield_2.5'
names(model_baseline)[names(model_baseline) == 'output_5'] <- 'quality_2.5'
names(model_baseline)[names(model_baseline) == 'output_6'] <- 'work_2.5'
names(model_baseline)[names(model_baseline) == 'output_7'] <- 'yield_4.5'
names(model_baseline)[names(model_baseline) == 'output_8'] <- 'quality_4.5'
names(model_baseline)[names(model_baseline) == 'output_9'] <- 'work_4.5'
names(model_baseline)[names(model_baseline) == 'output_10'] <- 'yield_8.5'
names(model_baseline)[names(model_baseline) == 'output_11'] <- 'quality_8.5'
names(model_baseline)[names(model_baseline) == 'output_12'] <- 'work_8.5'
#names(model_baseline)[names(model_baseline) == 'output_13'] <- ''
#names(model_baseline)[names(model_baseline) == 'output_14'] <- ''

#pivot long####
#compare yield
df_long_y <- model_baseline %>%
  pivot_longer(cols = starts_with("yield_"),
               names_to = "scenario",
               values_to = "yield_sim")

#compare quality
df_long_q <- model_baseline %>%
  pivot_longer(cols = starts_with("quality_"),
               names_to = "scenario",
               values_to = "quality_sim")
#compare work
df_long_w <- model_baseline %>%
  pivot_longer(cols = starts_with("work_"),
               names_to = "scenario",
               values_to = "work_sim")

#plots####
#yield
ggplot(df_long_y, aes(yield_sim, scenario, fill=scenario))+
  #geom_density(position = "identity")+
  geom_density_ridges()
#quality
ggplot(df_long_q, aes(quality_sim, scenario, fill=scenario))+
  #geom_density(position = "identity")+
  geom_density_ridges()
#work
ggplot(df_long_w, aes(work_sim, scenario, fill=scenario))+
  #geom_density(position = "identity")+
  geom_density_ridges()

#model2####
model_baseline2<-read.csv("results_m2/mcSimulationResults.csv")

names(model_baseline2)[names(model_baseline2) == 'output_1'] <- 'yield_baseline'
names(model_baseline2)[names(model_baseline2) == 'output_2'] <- 'quality_baseline'
names(model_baseline2)[names(model_baseline2) == 'output_3'] <- 'work_baseline'
names(model_baseline2)[names(model_baseline2) == 'output_4'] <- 'yield_2.5'
names(model_baseline2)[names(model_baseline2) == 'output_5'] <- 'quality_2.5'
names(model_baseline2)[names(model_baseline2) == 'output_6'] <- 'work_2.5'
names(model_baseline2)[names(model_baseline2) == 'output_7'] <- 'yield_4.5'
names(model_baseline2)[names(model_baseline2) == 'output_8'] <- 'quality_4.5'
names(model_baseline2)[names(model_baseline2) == 'output_9'] <- 'work_4.5'
names(model_baseline2)[names(model_baseline2) == 'output_10'] <- 'yield_8.5'
names(model_baseline2)[names(model_baseline2) == 'output_11'] <- 'quality_8.5'
names(model_baseline2)[names(model_baseline2) == 'output_12'] <- 'work_8.5'
#names(model_baseline2)[names(model_baseline2) == 'output_13'] <- ''
#names(model_baseline2)[names(model_baseline2) == 'output_14'] <- ''

#pivot long####
#compare yield
df_long_y <- model_baseline2 %>%
  pivot_longer(cols = starts_with("yield_"),
               names_to = "scenario",
               values_to = "yield_sim")

#compare quality
df_long_q <- model_baseline2 %>%
  pivot_longer(cols = starts_with("quality_"),
               names_to = "scenario",
               values_to = "quality_sim")
#compare work
df_long_w <- model_baseline2 %>%
  pivot_longer(cols = starts_with("work_"),
               names_to = "scenario",
               values_to = "work_sim")

#plots####
#yield
ggplot(df_long_y, aes(yield_sim, scenario, fill=scenario))+
  #geom_density(position = "identity")+
  geom_density_ridges()
#quality
ggplot(df_long_q, aes(quality_sim, scenario, fill=scenario))+
  #geom_density(position = "identity")+
  geom_density_ridges()
#work
ggplot(df_long_w, aes(work_sim, scenario, fill=scenario))+
  #geom_density(position = "identity")+
  geom_density_ridges()
