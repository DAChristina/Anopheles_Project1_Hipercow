# Data source: https://espen.afro.who.int/tools-resources/download-data
# Data downloaded in the middle of October 2023

# Brief technical summary:
# https://espen.afro.who.int/espen-2021-2025-pc-forecasts-brief-technical-summary
# Burkina Faso data for Lymphatic Filariasis, Site Level

library(tidyverse)

G_BF_ESPEN <- read.csv("raw_data/data-BF-LF-sitelevel.csv")

G_BF_ESPEN <- G_BF_ESPEN %>%
  dplyr::mutate(
    ADMIN1_NAME=str_replace(ADMIN1_NAME, "Boucle Du Mouhoun", "Boucle du Mouhoun"),
    ADMIN1_NAME=str_replace(ADMIN1_NAME, "Plateau Central", "Plateau-Central"),
    ADMIN1_NAME=str_replace(ADMIN1_NAME, "Hauts Bassins", "Haut-Bassins")
  ) %>% 
  dplyr::mutate(
    ADMIN2_NAME = str_replace(ADMIN2_NAME, "Boulmiougou,Nongrmassom", "Boulmiougou, Nongrmassom"),
    ADMIN2_NAME = str_replace(ADMIN2_NAME, "Reo,Tenado", "Reo, Tenado")
  ) %>% # I found some inconsistencies within the comma-separated values
  tidyr::separate_rows(ADMIN2_NAME, sep = ", ") %>% 
  tidyr::separate_rows(EU_NAME, sep = ", ") %>% # EU need information: not available on the tech. summary
  dplyr::mutate(
    IU_NAME = str_replace(IU_NAME, "Karangasso - Vigue", "Karangasso-Vigue"),
    IU_NAME = str_replace(IU_NAME, "Boulmiougou,Nongrmassom", "Boulmiougou, Nongrmassom"),
    IU_NAME = str_replace(IU_NAME, "Reo,Tenado", "Reo, Tenado"),
    IU_NAME = str_replace(IU_NAME, "Orodara,N'Dorola", "Orodara, N'Dorola")
  ) %>% # Again, some inconsistencies
  tidyr::separate_rows(IU_NAME, sep = ", ") %>% 
  dplyr::mutate(Year_start = as.numeric(Year_start))

# Forgot about the missing data through that gap year.
# Let's make another df with a complete timeline.
# Managing the data related to gap year
Year_anyone <- data.frame(Year = seq(min(G_BF_ESPEN$Year_start, na.rm = TRUE), 
                                     max(G_BF_ESPEN$Year_start, na.rm = TRUE)))

G_BF_ESPEN <- merge(Year_anyone, G_BF_ESPEN, by.x = "Year", by.y = "Year_start", all = TRUE)

# Confidence Interval Calculations #############################################
G_BF_ESPEN_Y <- G_BF_ESPEN %>% 
  filter(Year >= 2015,
         Positive != "null",
         Examined != "null") %>% 
  mutate(Prevalence = as.numeric(Prevalence),
         Examined = as.numeric(Examined),
         Positive = as.numeric(Positive),
         Calc_Prevalence = Positive/Examined,
         Conf_Int = binom.exact(Positive, Examined)) %>% 
  mutate(Prevalence = Prevalence) %>%  # Prevalence in per cent
  mutate(lo_CI = Conf_Int$lower,
         up_CI = Conf_Int$upper) # Show confint data

# See isoolated result:
G_BF_ESPEN_Y_isod <- G_BF_ESPEN_Y %>% 
  select(Year, ADMIN1_NAME, ADMIN2_NAME, LocationName, Latitude, Longitude, Positive, Examined, Prevalence, Calc_Prevalence, lo_CI, up_CI) %>% 
  filter(ADMIN1_NAME == "Sud-Ouest") %>% # Focused on South-west
  view() %>% 
  glimpse()


dir.create("inputs", FALSE, TRUE)
write.csv(G_BF_ESPEN, "inputs/data_BF_LF_sitelevel_cleaned.csv", row.names = TRUE)


