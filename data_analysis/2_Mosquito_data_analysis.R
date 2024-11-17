library(tidyverse)

dat_sel <- read.csv("inputs/Mosquito_EstablishedInfection_Data_clean.csv")

# Additional report for calculated mfE for mosquitoes with established infection
dat_mfE_calc <- dat_sel %>% # MUTATE below 
  dplyr::filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL),
                !is.na(Mosquito_larvae_infected_proportion_fromtotaldissected)) %>%
  dplyr::group_by(Reference, Mosquito_species) %>% 
  dplyr::mutate(mean_mfE = mean(mfE_mean_calculated),
                min_mfE = min(mfE_mean_calculated),
                max_mfE = max(mfE_mean_calculated),
                min_mosq_dissected = min(Mosquito_totaldissected),
                max_mosq_dissected = max(Mosquito_totaldissected),
                Larvae_mean = mean(Larvae_mean_calculated),
                mean_Mosquito_larvae_infected_proportion = mean(Mosquito_larvae_infected_proportion_fromtotaldissected),
                min_MosqInfected = min(Mosquito_larvae_infected_proportion_fromtotaldissected),
                max_MosqInfected = max(Mosquito_larvae_infected_proportion_fromtotaldissected)
                ) %>% 
  dplyr::distinct(Reference, Mosquito_species, .keep_all = T) %>% 
  # Can also be obtained by using dplyr::summarise() instead of a combination of dplyr::mutate & dplyr::distinct
  dplyr::select(Reference, Mosquito_species, mean_mfE, min_mfE, max_mfE, min_mosq_dissected, max_mosq_dissected,
                Larvae_mean, mean_Mosquito_larvae_infected_proportion, min_MosqInfected, max_MosqInfected
                ) %>%
  view() %>% 
  glimpse()

dat_mfE_report <- dat_mfE_calc %>% 
  dplyr::mutate(MeanRange_mfE = paste0(round(mean_mfE, 2), " (", round(min_mfE, 2), "-", round(max_mfE, 2), ") "),
                range_dissected_mosq = paste0(round(min_mosq_dissected, 2), "-", round(max_mosq_dissected, 2)),
                MeanRange_Mosq = paste0(round(mean_Mosquito_larvae_infected_proportion, 2), " (", round(min_MosqInfected, 2), "-", round(max_MosqInfected, 2), ") ")) %>% 
  dplyr::select(Reference, Mosquito_species, MeanRange_mfE, range_dissected_mosq, MeanRange_Mosq, Larvae_mean) %>% 
  view() %>% 
  glimpse()

dir.create("outputs", FALSE, TRUE)
write.csv(dat_mfE_report, "outputs/mosquitoes_report_table_reference.csv", row.names = TRUE)



# Additional descriptive reports
# 1. How many mosquitoes dissected & infected per-species?
desired_references <- unique(dat_mfE_report$Reference)

mosq_species <- dat_sel %>%
  dplyr::filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL),
                !is.na(Mosquito_larvae_infected_proportion_fromtotaldissected)) %>%
  # dplyr::filter(!is.na(Mosquito_larvae_infected_count)) %>% # Similar to those filters above!
  
  dplyr::group_by(Mosquito_species) %>%
  # Notes: I wanna analyse how many mosquitoes with ingested mf (mfE),
  # Not possible because studies directly dissected mosquitoes with developed larvae (not mfE)!
  dplyr::summarise(sum_dissected_mosquitoes = sum(Mosquito_totaldissected, na.rm = T),
                sum_larvae_infected_count_mosquitoes = sum(Mosquito_larvae_infected_count, na.rm = T),
                percentage_per_species = sum_larvae_infected_count_mosquitoes/sum_dissected_mosquitoes*100,
                mean_larvae_calculated = mean(Larvae_mean_arithmetic_perdissectedmosquito, na.rm = T),
                min_larvae_calculated = min(Larvae_mean_arithmetic_perdissectedmosquito, na.rm = T),
                max_larvae_calculated = max(Larvae_mean_arithmetic_perdissectedmosquito, na.rm = T)
  ) %>% 
  view() %>% 
  glimpse()

# trial from original data
# mosq_species_from_dat <- dat %>% 
#   dplyr::filter(Reference %in% desired_references) %>% 
#   dplyr::group_by(Mosquito_species) %>%
#   view()
#   dplyr::summarise(sum_dissected_mosquitoes = sum(Mosquito_totaldissected, na.rm = T),
#                    sum_mf_infected_count_mosquitoes = sum(Mosquito_mf_infected_count, na.rm = T),
#                    percentage_per_species = sum_mf_infected_count_mosquitoes/sum_dissected_mosquitoes*100,
#                    mean_mfE_calculated = mean(mfE_mean_calculated),
#                    min_mfE_calculated = min(mfE_mean_calculated),
#                    max_mfE_calculated = max(mfE_mean_calculated)
#   ) %>% 
#   view() %>% 
#   glimpse()

# 2. How many mosquitoes dissected & infected per-study?
mosq_study <- dat_sel %>% 
  dplyr::filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL),
                !is.na(Mosquito_larvae_infected_proportion_fromtotaldissected)) %>%
  # dplyr::filter(!is.na(Mosquito_larvae_infected_count)) %>% # Similar to those filters above!
  
  dplyr::group_by(Reference) %>%
  # Notes: I wanna analyse how many mosquitoes with ingested mf (mfE),
  # Not possible because studies directly dissected mosquitoes with developed larvae (not mfE)!
  dplyr::summarise(sum_dissected_mosquitoes = sum(Mosquito_totaldissected, na.rm = T),
                   sum_larvae_infected_count_mosquitoes = sum(Mosquito_larvae_infected_count, na.rm = T),
                   percentage_per_study = sum_larvae_infected_count_mosquitoes/sum_dissected_mosquitoes*100,
                   mean_larvae_calculated = mean(Larvae_mean_arithmetic_perdissectedmosquito, na.rm = T),
                   min_larvae_calculated = min(Larvae_mean_arithmetic_perdissectedmosquito, na.rm = T),
                   max_larvae_calculated = max(Larvae_mean_arithmetic_perdissectedmosquito, na.rm = T)
  ) %>% 
  view() %>% 
  glimpse()


