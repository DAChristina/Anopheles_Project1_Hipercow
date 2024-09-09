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
         max_MosqInfected = max(Mosquito_larvae_infected_proportion_fromtotaldissected)) %>% 
  dplyr::distinct(Reference, Mosquito_species, .keep_all = TRUE) %>% 
  dplyr::select(Reference, Mosquito_species, mean_mfE, min_mfE, max_mfE, min_mosq_dissected, max_mosq_dissected, Larvae_mean, mean_Mosquito_larvae_infected_proportion, min_MosqInfected, max_MosqInfected) %>% 
  # view() %>% 
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
