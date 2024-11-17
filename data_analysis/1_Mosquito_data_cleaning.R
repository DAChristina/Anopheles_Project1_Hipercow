# The proportion of mosquitoes with established infection

# Contents: ####################################################################
# 1. Data preparation
# 2. Visualisation

# 1. Data preparation ##########################################################
library(tidyverse)
library(readxl)

dat <- readxl::read_excel('raw_data/Mosquito_EstablishedInfection_Data.xlsx') %>% 
  # view() %>% 
  glimpse()

dat_sel <- dat %>% 
  dplyr::select(Reference, Human_mf_intensity_per1mL, Human_mf_intensity_per20uL, Mosquito_species, Mosquito_totaldissected, Mosquito_mf_infected_count, Mosquito_mf_infected_proportion, mf_mean_arithmetic_pertotaldissectedmosquito, Mosquito_larvae_infected_count, Mosquito_larvae_infected_proportion_fromtotaldissected, Larvae_mean_arithmetic_perdissectedmosquito) %>% 
  dplyr::filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>% # Filter out info (e.g. "Captured mosquitoes," etc.)
  dplyr::filter(grepl("^[0-9.]+$", Mosquito_totaldissected)) %>% # Used data based on mfH & total dissected of mosquitoes
  # dplyr::filter(Mosquito_species != "An. melas") %>% # Omit An. melas
  dplyr::mutate(Human_mf_intensity_per1mL = as.numeric(Human_mf_intensity_per1mL),
                Human_mf_intensity_per20uL = as.numeric(Human_mf_intensity_per20uL),
                Mosquito_totaldissected = as.numeric(Mosquito_totaldissected),
                Mosquito_mf_infected_count = as.numeric(Mosquito_mf_infected_count),
                Mosquito_mf_infected_proportion = as.numeric(Mosquito_mf_infected_proportion),
                mf_mean_arithmetic_pertotaldissectedmosquito = as.numeric(mf_mean_arithmetic_pertotaldissectedmosquito),
                Mosquito_larvae_infected_count = as.numeric(Mosquito_larvae_infected_count),
                Mosquito_larvae_infected_proportion_fromtotaldissected = as.numeric(Mosquito_larvae_infected_proportion_fromtotaldissected), 
                Larvae_mean_arithmetic_perdissectedmosquito = as.numeric(Larvae_mean_arithmetic_perdissectedmosquito)) %>% 
  
  # Calculations based on Snow et al (2002 & 2006)
  dplyr::mutate(Human_mf20uL_log_calculated = .1149*(log10(Human_mf_intensity_per1mL+1))^2 + .037*(log10(Human_mf_intensity_per1mL+1)) - .0309,
                Human_mf20uL_calculated = 10^Human_mf20uL_log_calculated,
                mfE_log_mean_calculated = log10(-.0061+1)+.4010*log10(Human_mf_intensity_per20uL+1), # log10(mfE(mfH)) = log10(-0.0061+1)+0.4010*log10(mfH+1)
                mfE_mean_calculated = 10^mfE_log_mean_calculated,
                Larvae_mean_calculated = (.03*mfE_mean_calculated^2.3520)/(1+((mfE_mean_calculated^2.3520)/482.5680))) %>% 
  # mutate(MosquitoInfected_calculated = ) # Analyse this part by using MLE
  glimpse()

write.csv(dat_sel, "inputs/Mosquito_EstablishedInfection_Data_clean.csv", row.names = TRUE)
