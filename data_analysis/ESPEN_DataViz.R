# 2023 Geo spatial data for Burkina Faso (Including prevalence, ehe~)

# GADM codes of subdivisions:
# 0 = country
# 1 = provinces
# 2 = regencies & cities
# 3 = villages?
# 4 = vvilllagess???
# --> Point (4) makes your computer run SLOWLY!\
# Consider to use filter prior using this.

################################################################################
# 1. INTRO: Load the Geo spatial file
# TRIAL to use a shapefile (*.shp) files first!
################################################################################

wd = "C:/Users/dac23/Downloads"
setwd(wd)

# 1. Unzip file coz' file downloaded from GADM is in *.zip
# Linux
# BF_shp_zip = '/home/ron/Downloads/2023 Imperial MRes Journey/2023 Project1LF Mod TRANSFIL/BF Map/gadm41_BFA_shp.zip'
# BF_shp_out = '/home/ron/Downloads/2023 Imperial MRes Journey/2023 Project1LF Mod TRANSFIL/BF Map/gadm41_BFA_shp'

# Trashy WindOS
BF_shp_zip = 'C:/Users/dac23/Downloads/gadm41_BFA_shp.zip'
BF_shp_out = 'C:/Users/dac23/Downloads/gadm41_BFA_shp'


unzip (BF_shp_zip, exdir=BF_shp_out)

# 2. Load the file, we use GADM code 4 = vvilllagess???
# BF_shp_path_LINUX = '/home/ron/Downloads/2023 Imperial MRes Journey/2023 Project1LF Mod TRANSFIL/BF Map & Data/gadm41_BFA_shp/gadm41_BFA_3.shp'
BF_shp_path_LINUX = 'C:/Users/dac23/Downloads/gadm41_BFA_shp/gadm41_BFA_3.shp'

library(tidyverse)
library(sf) # Karena tidyverse ga bisa wrangling data geospasial.

BF_spdf <- st_read(dsn = BF_shp_path_LINUX)

# summary(BF_spdf) # tells you the max and min coordinates, the kind of projection in use
# length(BF_spdf) # how many regions you have

# Coz' this is an sf object
head(sf::st_drop_geometry(BF_spdf))
glimpse(BF_spdf)

# Compare NAME 1 vs. NAME 2, just being curious to find LONDON
unique(BF_spdf$NAME_1) # Name 1 = 13 regions
unique(BF_spdf$NAME_2) # Name 2 = 45 provinces (Kyelem's Dissertation, 2007)
unique(BF_spdf$NAME_3) # Name 3 = 349 Departments (350 in Kyelem's)

# NEXT:
# U have to combine *.csv OR *.xlsx data to *.shp (CANNOT BE RUN VICE-VERSA)!!!
# Convert the 'SpatialPolygonsDataFrame' to 'sf' object first!

# Now, let's analyse the *.csv OR *.xlsx data!

################################################################################
# 2. Load & Data wrangling for Burkina Faso (BF), 2 datasets available:
# 2.1. Based on sitelevel
# 2.2. Based on IU (Implementation Unit)

# Specifically I want to highlight the prevalence (for each year), in 2 categories:
# 1. National level (grouped by year)--> Mean, Med, Mo, Max, Min (sum examined/sum total)
# 1.1. Table
# 1.2. Histograms

# 2. Regional level (grouped by year, 13 regions)--> Mean, Med, Mo, Max, Min (sum examined/sum total)
# 2.1. Table
# 2.2. Heatmap
# 2.3. Map each year (from 2020-2023)
# 2.4. Map that shows GPS point where the 5 highest prevalence found in each region
# I'll think about anything else.

# Workflow:
# 1. Create/load the df
# 2. Filter to the specific:
# 2.1. Year range
# 2.2. Regions with high prevalence in the last x years (start form 2010 or 2015)
# 3. Data preparation for GADM map
# 4. Combine G_BF_ESPEN_Admin1_forGADM to *.shp data (CANNOT BE RUN VICE-VERSA)!!!
################################################################################
# SOURCE: https://espen.afro.who.int/tools-resources/download-data

# Brief technical summary:
# https://espen.afro.who.int/espen-2021-2025-pc-forecasts-brief-technical-summary
# Burkina Faso data for Lymphatic Filariasis, Site Level

G_BF_ESPEN_Path_LINUX = "C:/Users/dac23/Downloads/data-BF-LF-sitelevel.csv"
G_BF_ESPEN <- read_csv(G_BF_ESPEN_Path_LINUX, col_names = T)
# view(G_BF_ESPEN)
head(G_BF_ESPEN)
unique(G_BF_ESPEN$ADMIN1_NAME)
unique(G_BF_ESPEN$ADMIN2_NAME)
unique(G_BF_ESPEN$EU_NAME)
unique(G_BF_ESPEN$IU_NAME)
unique(G_BF_ESPEN$LocationName)
unique(G_BF_ESPEN$NamePositioning)

# U see, ADMIN2_NAME etc have multiple values, let's separate it first:

G_BF_ESPEN <- G_BF_ESPEN %>%
  mutate(
    ADMIN2_NAME = str_replace(ADMIN2_NAME, "Boulmiougou,Nongrmassom", "Boulmiougou, Nongrmassom"),
    ADMIN2_NAME = str_replace(ADMIN2_NAME, "Reo,Tenado", "Reo, Tenado")
  ) %>% # I found some inconsistencies within the comma-separated values
  separate_rows(ADMIN2_NAME, sep = ", ") %>% 
  separate_rows(EU_NAME, sep = ", ") %>% # EU need information: not available on the tech. summary
  mutate(
    IU_NAME = str_replace(IU_NAME, "Karangasso - Vigue", "Karangasso-Vigue"),
    IU_NAME = str_replace(IU_NAME, "Boulmiougou,Nongrmassom", "Boulmiougou, Nongrmassom"),
    IU_NAME = str_replace(IU_NAME, "Reo,Tenado", "Reo, Tenado"),
    IU_NAME = str_replace(IU_NAME, "Orodara,N'Dorola", "Orodara, N'Dorola")
  ) %>% # Again, some inconsistencies
  separate_rows(IU_NAME, sep = ", ") #%>%
  # separate_rows(LocationName, sep = ", ") %>% # Searched some of them online, might be communities
  # separate_rows(NamePositioning, sep = ", ") # Some of them could be local areas/communities

# Check the unique values in the ADMIN2_NAME column, etc.
unique(G_BF_ESPEN$IU_NAME) # Let's be focused on ADMIN1, 2, EU and IU

# ADMIN2_NAME vs. IU_NAME
NeedToBeCorrected_IU_NAME <- G_BF_ESPEN[!G_BF_ESPEN$IU_NAME %in% G_BF_ESPEN$ADMIN2_NAME,]
unique(NeedToBeCorrected_IU_NAME$IU_NAME)

NeedToBeCorrected_ADMIN2_NAME <- G_BF_ESPEN[!G_BF_ESPEN$ADMIN2_NAME %in% G_BF_ESPEN$IU_NAME,]
unique(NeedToBeCorrected_ADMIN2_NAME$ADMIN2_NAME)

# Again, some inconsistencies:
# IU_NAME "Karangasso-Vigue"  - Department
# IU_NAME "Kampti"            - Province
# IU_NAME "Garango-Pouytenga" - Department
# IU_NAME "Kombissiri-Sapone" - Department
# IU_NAME "Koudougou-Nanoro"  - Department
# IU_NAME "Bogande-Manni"     - Department
# IU_NAME "Fada N'gourma"     - Province
# IU_NAME "Bogodogo (p. rrl)" - Department
# ADMIN2_NAME "Bogange"           - ???
# ADMIN2_NAME "Karangasso Vigue"  - Department (typographical error)
# ADMIN2_NAME "Barsalogo"         - Could be a community/village?

# It seems like:
# 13 regions      = Name 1 (GADM) = ADMIN1_NAME (ESPEN)
# 45 provinces    = Name 2 (GADM) = ???
# 349 Departments = Name 3 (GADM) = ADMIN1_NAME & IU_NAME (ESPEN, but some of them are not Dept.)

# Quick notes: an error occur in columns (26, 27) rows (1364, 1366, 1434, 1436)
# They should be filled with 'year' but 'null' instead.
# I'll filter the value for further analysis.

# Let's check name consistency in GADM database AND ESPEN data:
# ADMIN1_NAME vs. NAME_1
NeedToBeCorrected_ESPEN <- G_BF_ESPEN[!G_BF_ESPEN$ADMIN1_NAME %in% BF_spdf$NAME_1,]
unique(NeedToBeCorrected_ESPEN$ADMIN1_NAME)

NeedToBeCorrected_GADM <- BF_spdf[!BF_spdf$NAME_1 %in% G_BF_ESPEN$ADMIN1_NAME,]
unique(NeedToBeCorrected_GADM$NAME_1)
# I suggest to change the GADM data,
# But we'll keep it for later since modifying *.shp files need library(sp)
# Now, we only change the ESPEN data so both files can be merged!

# FILTER & data wrangling the ESPEN file:
G_BF_ESPEN <- G_BF_ESPEN %>% 
  mutate(ADMIN1_NAME=str_replace(ADMIN1_NAME, "Boucle Du Mouhoun", "Boucle du Mouhoun")) %>% 
  mutate(ADMIN1_NAME=str_replace(ADMIN1_NAME, "Plateau Central", "Plateau-Central")) %>% 
  mutate(ADMIN1_NAME=str_replace(ADMIN1_NAME, "Hauts Bassins", "Haut-Bassins"))

unique(G_BF_ESPEN$ADMIN1_NAME)

# I tried any other combinations and have not found which column represents:
# 1. 45 provinces in ESPEN data (NAME_2 in GADM)
# 2. 349 Departments in ESPEN data (NAME_3 in GADM),
# could be ADMIN2_NAME & IU_NAME, but some of them are not Dept.

# TRIAL
# IU_NAME vs. NAME_3
NeedToBeCorrected_ESPEN <- G_BF_ESPEN[!G_BF_ESPEN$IU_NAME %in% BF_spdf$NAME_3,]
unique(NeedToBeCorrected_ESPEN$IU_NAME)

NeedToBeCorrected_GADM <- BF_spdf[!BF_spdf$NAME_3 %in% G_BF_ESPEN$IU_NAME,]
unique(NeedToBeCorrected_GADM$NAME_3)

# So, let's stick to the 13 regions
glimpse(G_BF_ESPEN)

# So..... Filtering now? How about by year? #####
# Checking years variable one-by-one

unique(G_BF_ESPEN$Year_start)
unique(G_BF_ESPEN$Year_end)
unique(G_BF_ESPEN$Year_MDA) # Could be from different dataset (2000-2005)

# What I can do is checking whether Year_start == Year_end:
Corr_Year_start <- G_BF_ESPEN[!G_BF_ESPEN$Year_start %in% G_BF_ESPEN$Year_end,]
unique(Corr_Year_start$Year_start)

Corr_Year_end <- G_BF_ESPEN[!G_BF_ESPEN$Year_end %in% G_BF_ESPEN$Year_start,]
unique(Corr_Year_end$Year_end)
# I found that Year_start == Year_end (unless a data in 1970 on Year_end)

# There is null data in Year_start, supposedly 2023. Wanna correct this first:
G_BF_ESPEN <- G_BF_ESPEN %>%
  mutate(Year_start = ifelse(is.na(Year_start), 2023, Year_start))
unique(G_BF_ESPEN$Year_start)


# Forgot about the missing data through that gap year.
# Let's make another df with a complete timeline.
# Managing the data related to gap year
Year_anyone <- data.frame(Year = seq(min(G_BF_ESPEN$Year_start, na.rm = TRUE), 
                                     max(G_BF_ESPEN$Year_start, na.rm = TRUE))) %>% 
  # view() %>% 
  glimpse()

# This will change the Year_start into Year, some data might be null.
G_BF_ESPEN <- merge(Year_anyone, G_BF_ESPEN, by.x = "Year", by.y = "Year_start", all = TRUE)
view(G_BF_ESPEN)
# So the year is available; now it is called, "Year" NOT "Year_start"
# How about we stick to Date_entry. If Date_entry = null, fill all of the colums with numerical zero.
G_BF_ESPEN[is.na(G_BF_ESPEN$Date_entry), -1] <- 0 #-1 means ignore Year (first column), if not, year would be zero -_-)
view(G_BF_ESPEN)

# NATIONAL PREVALENCE ##########################################################
# What I wanna do is create a new df, grouped the data by year & make a summary
# QUICK NOTES:
# I tried to add & RENEW year data, but it's not working, right...

# Recall G_BF_ESPEN_Y UPDATE CONFIDENCE INTERVAL ###############################
library(epitools)
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

ggplot(G_BF_ESPEN_Y_isod, aes(x = Year, y = Calc_Prevalence, ymin = lo_CI, ymax = up_CI)) +
  geom_point(shape=21, fill="lightblue", color="black", size=3) +
  geom_hline(yintercept=.01, linetype="dashed", color = "red") + # WHO proposed 1%
  geom_hline(yintercept=.02, linetype="dashed", color = "red") + # WHO proposed 2%
  facet_wrap(~ ADMIN2_NAME) +
  labs(x = "Year", y = "Prevalence (in proportion)", title = "The prevalence of mf in South-west Region (2015-2021)") +
  geom_segment(aes(x = Year, xend = Year, y = lo_CI, yend = up_CI), color = "darkred") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA))

# The old one:
# ACTIVATE THIS FILTER IF FOCUSED ON >= 2015
G_BF_ESPEN_Y <- G_BF_ESPEN %>% 
  filter(Year >= 2015) %>% 
  mutate(Prevalence = as.numeric(Prevalence),
         Examined = as.numeric(Examined),
         Positive = as.numeric(Positive),
         Calc_Prevalence = Positive/Examined*100) %>% 
  mutate(Prevalence = Prevalence*100) # Prevalence in per cent



# t-test LOOP for each year
res_df <- data.frame() # R: have to create an empty storage!
for (i in unique(G_BF_ESPEN_Y$Year)) {
  dat <- subset(G_BF_ESPEN_Y, Year == i)
  res <- t.test(dat$Prevalence, dat$Calc_Prevalence)
  
  comp_res <- data.frame(
    Year = i,
    t_statistic = res$statistic,
    df = res$parameter,
    p_value = res$p.value,
    lower_CI = res$conf.int[1],
    upper_CI = res$conf.int[2],
    mean_Prev = res$estimate[1],
    mean_Prev = res$estimate[2]
  )
  res_df <- rbind(res_df,comp_res)
}

view(res_df)

G_BF_ESPEN_Mean_Prev <- G_BF_ESPEN_Y %>% 
  filter(Prevalence != 'null') %>%
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  group_by(Year) %>% 
  summarise(Prevalence = mean(Prevalence)) %>% 
  ungroup() %>%
  # view() %>% 
  glimpse()

# G_BF_ESPEN <- G_BF_ESPEN %>% mutate(Year = as.numeric(Year))
# Simple plot of national prevalence by average
plot(G_BF_ESPEN_Mean_Prev$Year, G_BF_ESPEN_Mean_Prev$Prevalence,
     type = "p", pch = 16, cex = 1,
     xlab = "Year", ylab = "Prevalence", main = "Average Prevalence")

# Boxplot? I'd rather use base R instead of ggplot -_-)
# Data needed:
# Change the 0 values to NA coz' NO DATA not zero prevalence!
G_BF_ESPEN_Y$Prevalence <- as.numeric(G_BF_ESPEN_Y$Prevalence)

# Missing gap year issue (x-axis):
# I don't know why when I activate this line below the data cropped in Boxplot but NOT in ggplot -_-)
# Somehow it worked well in the past but when I re-run the code it only occur when gap year = 0
# G_BF_ESPEN_Y$Prevalence[G_BF_ESPEN_Y$Prevalence == 0] <- NA # Have checked that 1969-1999 (gap year) = 0, so this would be safe
G_BF_ESPEN_Mean_Prev$Prevalence[G_BF_ESPEN_Mean_Prev$Prevalence == 0] <- NA # Have checked that 1969-1999 (gap year) = 0, so this would be safe


boxplot(Prevalence ~ Year, data = G_BF_ESPEN_Y, 
        xlab = "Year", ylab = "Prevalence (%)",
        main = "The National Prevalence of Lymphatic Filariasis in Burkina Faso from 2015 to 2023", col = "lightblue")
abline(h = 1, col = "red", lty = 2)
abline(h = 2, col = "red", lty = 2)
# Add prevalence points
points(as.factor(unique(G_BF_ESPEN_Mean_Prev$Year)), G_BF_ESPEN_Mean_Prev$Prevalence, pch = 5, col = "black", cex = 1)

# TRIAL CI calculation for national prevalence
G_BF_ESPEN_Mean_Prev <- G_BF_ESPEN_Y %>% 
  filter(Method_1 != "Clinical",
         Prevalence != 'null') %>%
  mutate(Prevalence = as.numeric(Prevalence),
         Positive = as.numeric(Positive),
         Examined = as.numeric(Examined)) %>% 
  filter(!is.na(Prevalence),
         !is.na(Positive),
         !is.na(Examined)) %>% 
  group_by(Year) %>% 
  summarise(Prevalence = mean(Prevalence)/100, # Because the data above use prevalence in (%)
            Sum_positive = sum(Positive),
            Sum_examined = sum(Examined)) %>% 
  ungroup() %>%
  view() %>% 
  glimpse()

# CI by using binom.exact can't be used because some data value have NO INFO about sample size and positive (ONLY PREVALENCE IS AVAILABLE)
library(epitools)
CI_trial <- binom.exact(G_BF_ESPEN_Mean_Prev$Sum_positive, G_BF_ESPEN_Mean_Prev$Sum_examined, conf.level = 0.95)
G_BF_ESPEN_comb <- left_join(G_BF_ESPEN_Mean_Prev, CI_trial,
                             by = c("Sum_positive" = "x", "Sum_examined" = "n")) %>%
  rename(ESPEN_Prev = Prevalence,       # Prevalence from ESPEN, but with NO data for positives and total samples
         Calc_Prev = proportion) %>%    # Calc Prevalence from ESPEN with available positives and total samples
  # view() %>% 
  glimpse()

# Trying to compare the data by using t student's:
Prev_t_compare <- t.test(G_BF_ESPEN_comb$ESPEN_Prev, G_BF_ESPEN_comb$Calc_Prev)
Prev_t_compare

# How many nulls?
HowMany_Nulls <- G_BF_ESPEN_Y %>% 
  select(Method_1, Examined, Year) %>% 
  filter(Method_1 != "Clinical",
         Examined == "null") %>% 
  group_by(Year) %>% 
  count() %>% 
  ungroup() %>% 
  view()


# How many IUs screened from 2015-2023?
HowMany_IUs_Sero <- G_BF_ESPEN_Y %>% 
  select(Method_1, Method_2, Examined, Year, ADMIN1_NAME, ADMIN2_NAME, IU_NAME, Prevalence) %>% 
  filter(Method_1 != "Clinical",
         !is.na(Prevalence)) %>% 
  group_by(Year, ADMIN1_NAME, ADMIN2_NAME, Method_1, Method_2) %>% # ADMIN2_NAME = IU_NAME
  summarise(mean_Prev = mean(Prevalence)) %>% # count() %>% 
  ungroup() %>% 
  mutate(
    ADMIN1_NAME = case_when(
      ADMIN1_NAME == 'Centre-Est' ~ 'Center-East',
      ADMIN1_NAME == 'Haut-Bassins' ~ 'Upper Basins',
      ADMIN1_NAME == 'Nord' ~ 'North',
      ADMIN1_NAME == 'Centre-Nord' ~ 'Center-North',
      ADMIN1_NAME == 'Centre' ~ 'Center',
      ADMIN1_NAME == 'Boucle du Mouhoun' ~ 'Mouhoun Loop',
      ADMIN1_NAME == 'Est' ~ 'East',
      ADMIN1_NAME == 'Centre-Sud' ~ 'Center-South',
      ADMIN1_NAME == 'Cascades' ~ 'Cascades',
      ADMIN1_NAME == 'Sud-Ouest' ~ 'South-West',
      ADMIN1_NAME == 'Centre-Ouest' ~ 'Center-West',
      ADMIN1_NAME == 'Plateau-Central' ~ 'Central Plateau',
      ADMIN1_NAME == 'Sahel' ~ 'Sahel',
      TRUE ~ ADMIN1_NAME
    )) %>% 
  view()

# How about a plot? -_-)
# plot(HowMany_IUs_Sero$Year, HowMany_IUs_Sero$n)

# Since when they stop using Parasitological (thick blood smear)?
Smear_Stop <- G_BF_ESPEN_Y %>% 
  select(Method_1, Examined, Year, ADMIN1_NAME, ADMIN2_NAME, IU_NAME, Prevalence) %>% 
  filter(Method_1 == "Parasitological",
         !is.na(Prevalence)) %>% 
  group_by(Year) %>% 
  count() %>% 
  ungroup() %>% 
  view() %>% 
  glimpse()

# They're still using blood smear in some survey even in 2023

# REGIONAL PREVALENCE ##########################################################
# What I wanna do is create a new df, grouped the data by year AND regional & make a summary
# Change the "Year" filter 
all_ADMIN1_NAME <- unique(G_BF_ESPEN$ADMIN1_NAME) # For legend Regional colours
G_BF_ESPEN_Mean_Prev_Regional <- G_BF_ESPEN %>% 
  filter(ADMIN1_NAME == c('Centre-Est','Est','Sud-Ouest')) %>% 
  filter(Prevalence != 'null') %>%
  filter(Prevalence != 0) %>% # FILTER OUT for focused on positive results
  filter(Year >= 2021) %>% # Change the year filter to get specified timeframe
  mutate(
    ADMIN1_NAME = case_when(
      ADMIN1_NAME == 'Centre-Est' ~ 'Center-East',
      ADMIN1_NAME == 'Est' ~ 'East',
      ADMIN1_NAME == 'Sud-Ouest' ~ 'South-West',
      TRUE ~ ADMIN1_NAME
    )) %>% 
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  group_by(Year,ADMIN1_NAME) %>% 
  summarise(Prevalence = mean(Prevalence)) %>% 
  ungroup() %>%
  subset(ADMIN1_NAME != "0")

# glimpse(G_BF_ESPEN_Mean_Prev_Regional_TRIAL)
# Save table to *.csv from 2015 data to 2023
write.csv(G_BF_ESPEN_Mean_Prev_Regional, "BF_Average_Prevalence_2Regional_1BarPlot_42015-2023.csv", row.names = FALSE)

# Barplot (Can't use stacked here)
ggplot(G_BF_ESPEN_Mean_Prev_Regional, aes(x = Year, y = Prevalence, fill = ADMIN1_NAME)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Prevalence", fill = "Region") +
  scale_fill_manual(values = rainbow(length(all_ADMIN1_NAME))) +
  theme_minimal()

# Seems like from 2017 there are 4% occurence.
# How about we create small multiple table from 2017-2023?
library(RColorBrewer)
ggplot(G_BF_ESPEN_Mean_Prev_Regional, aes(x = Year, y = Prevalence, fill = ADMIN1_NAME)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Year", y = "Prevalence", fill = "Region") +
  # scale_fill_manual(values = brewer.pal(n = length(unique(G_BF_ESPEN_Mean_Prev_Regional$ADMIN1_NAME)), name = "Blues")) +
  scale_fill_manual(values = c('lightblue','lightblue','lightblue')) +
  theme_minimal() +
  facet_wrap(~ADMIN1_NAME) +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.02, linetype = "dashed", color = "red") +
  scale_color_manual(name = " ", 
                     values = "red", 
                     breaks = "WHO 1% cutoff", 
                     labels = c("WHO 1% cutoff")) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  guides(fill = FALSE) # DELETE the legends

# A summary data about sum up ALL kind of prevalence since 2017
# I checked that all of them is "PoT = Postintervention"
G_BF_ESPEN_2017 <- G_BF_ESPEN %>% 
  # filter(Prevalence != 'null') %>%
  filter(Year >= 2015) %>% 
  # mutate(Prevalence = as.numeric(Prevalence)) %>% 
  # group_by(Year) %>% 
  # summarise(Prevalence = mean(Prevalence)) %>% 
  # ungroup() %>%
  view()

# How about decision summary per-year, per-region?

# Total decision per-year, per-region
G_BF_ESPEN_2017_TotalDecision <- G_BF_ESPEN_2017 %>% 
  select(Year, ADMIN1_NAME, Prevalence, Decision) %>% 
  group_by(Year, ADMIN1_NAME) %>% 
  summarise(Decision_TOTAL = length(Decision)) %>% 
  ungroup() %>% 
  # view() %>% 
  glimpse()

G_BF_ESPEN_2017_Decision <- G_BF_ESPEN_2017 %>% 
  select(Year, ADMIN1_NAME, Prevalence, Decision) %>% 
  group_by(Year, ADMIN1_NAME, Decision) %>% 
  summarise(Decision_count = length(Decision)) %>% 
  ungroup() %>% 
  # view() %>% 
  glimpse()

G_BF_ESPEN_2017_Decision_Percent <- right_join(G_BF_ESPEN_2017_Decision, G_BF_ESPEN_2017_TotalDecision, by = c("Year", "ADMIN1_NAME"),) %>% 
  filter(ADMIN1_NAME == 'Centre-Est' | ADMIN1_NAME == 'Centre' | ADMIN1_NAME == 'Est' | ADMIN1_NAME == 'Sud-Ouest') %>% # Focused on regions with high prevalence
  mutate(Decision_Percent = Decision_count/Decision_TOTAL) %>% 
  mutate(
    ADMIN1_NAME = case_when(
      ADMIN1_NAME == 'Centre-Est' ~ 'Center-East',
      ADMIN1_NAME == 'Haut-Bassins' ~ 'Upper Basins',
      ADMIN1_NAME == 'Nord' ~ 'North',
      ADMIN1_NAME == 'Centre-Nord' ~ 'Center-North',
      ADMIN1_NAME == 'Centre' ~ 'Center',
      ADMIN1_NAME == 'Boucle du Mouhoun' ~ 'Mouhoun Loop',
      ADMIN1_NAME == 'Est' ~ 'East',
      ADMIN1_NAME == 'Centre-Sud' ~ 'Center-South',
      ADMIN1_NAME == 'Cascades' ~ 'Cascades',
      ADMIN1_NAME == 'Sud-Ouest' ~ 'South-West',
      ADMIN1_NAME == 'Centre-Ouest' ~ 'Center-West',
      ADMIN1_NAME == 'Plateau-Central' ~ 'Central Plateau',
      ADMIN1_NAME == 'Sahel' ~ 'Sahel',
      TRUE ~ ADMIN1_NAME
    )) %>% 
  # filter(Decision != "null") %>% # for table I wanna filter out NULL
  # view() %>% 
  glimpse()

# Save table to *.csv from 2017 data to 2023
write.csv(G_BF_ESPEN_2017_Decision_Percent, "BF_Decision_2Regional_TABLE_52017-2023.csv", row.names = FALSE)

# Di-plot aja deh, mabok liat tabel.
# Stacked barplot or facet_wrap?
ggplot(G_BF_ESPEN_2017_Decision_Percent, aes(x = Year, y = Decision_Percent, fill = Decision)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "The composition of decision", fill = "Region") +
  scale_fill_manual(values = c("darkred","lightgreen","grey","steelblue")) + #if there is null use "grey" for null
  theme_minimal() +
  facet_wrap(~ADMIN1_NAME) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

################################################################################
# I think we can't work with sitelevel data, load IU data instead: #############
################################################################################
G_BF_IU_Path = "C:/Users/dac23/Downloads/data-BF-LF-iu.csv"
G_BF_IU <- read_csv(G_BF_IU_Path, col_names = T)
# view(G_BF_ESPEN)
glimpse(G_BF_IU)
unique(G_BF_IU$ADMIN1)
unique(G_BF_IU$ADMIN2)
unique(G_BF_IU$IUs_NAME)

# Feels like ADMIN2 == IUs_NAME? Yup, ADMIN2 == IUs_Name
G_BF_IU <- G_BF_IU %>%
  mutate(ADMIN2_is_IUs_NAME = case_when(
    ADMIN2 == IUs_NAME ~ TRUE,
    TRUE ~ FALSE
  ))
unique(G_BF_IU$ADMIN2_is_IUs_NAME)

# Select data for plot

G_BF_IU_2017 <- G_BF_IU %>% 
  # filter(Prevalence != 'null') %>%
  filter(Year >= 2015) %>% # Trying to filter out Year but I suppose it doesn't required.
  # mutate(Prevalence = as.numeric(Prevalence)) %>% 
  # group_by(Year) %>% 
  # summarise(Prevalence = mean(Prevalence)) %>% 
  # ungroup() %>%
  # view() %>% 
  glimpse()

# Total decision per-year, per-region
G_BF_IU_2017_TotalDecision <- G_BF_IU_2017 %>% 
  select(Year, ADMIN1, Endemicity, MDA_scheme) %>% 
  group_by(Year, ADMIN1) %>% 
  summarise(Decision_TOTAL = length(Endemicity)) %>% 
  ungroup() %>% 
  # view() %>% 
  glimpse()

G_BF_IU_2017_Decision <- G_BF_IU_2017 %>% 
  select(Year, ADMIN1, Endemicity, MDA_scheme) %>% 
  group_by(Year, ADMIN1, Endemicity) %>% 
  summarise(Decision_count = length(Endemicity)) %>% 
  ungroup() %>% 
  # view() %>% 
  glimpse()

# Make a new df about summary of decisions (percent)
G_BF_IU_2017_Decision_Percent <- right_join(G_BF_IU_2017_Decision, G_BF_IU_2017_TotalDecision, by = c("Year", "ADMIN1"),) %>% 
  mutate(Decision_Percent = Decision_count/Decision_TOTAL) %>% 
  mutate(
    ADMIN1 = case_when(
      ADMIN1 == 'CENTRE-EST' ~ 'Center-East',
      ADMIN1 == 'HAUTS BASSINS' ~ 'Upper Basins',
      ADMIN1 == 'NORD' ~ 'North',
      ADMIN1 == 'CENTRE-NORD' ~ 'Center-North',
      ADMIN1 == 'CENTRE' ~ 'Center',
      ADMIN1 == 'BOUCLE DU MOUHOUN' ~ 'Mouhoun Loop',
      ADMIN1 == 'EST' ~ 'East',
      ADMIN1 == 'CENTRE-SUD' ~ 'Center-South',
      ADMIN1 == 'CASCADES' ~ 'Cascades',
      ADMIN1 == 'SUD-OUEST' ~ 'South-West',
      ADMIN1 == 'CENTRE- OUEST' ~ 'Center-West',
      ADMIN1 == 'PLATEAU CENTRAL' ~ 'Central Plateau',
      ADMIN1 == 'SAHEL' ~ 'Sahel',
      TRUE ~ ADMIN1
    )) %>% 
  # filter(Decision != "null") %>% # for table I wanna filter out NULL
  # view() %>% 
  glimpse()

ggplot(G_BF_IU_2017_Decision_Percent, aes(x = Year, y = Decision_Percent, fill = Endemicity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Proportion of endemicity and decision",
       # main = "The composition of endemicity and decision for each region",
       fill = "Region") +
  scale_fill_manual(values = c("darkred","lightgreen","steelblue","grey")) + #if there is null use "grey" for null
  theme_minimal() +
  facet_wrap(~ADMIN1) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# TRIAL grouped by ADMIN2 (IMPLEMENTATION UNIT NAMES)
# Total decision per-year, per-region
G_BF_IU_2017_TotalDecision <- G_BF_IU_2017 %>% 
  select(Year, ADMIN1, ADMIN2, Endemicity, MDA_scheme) %>% 
  group_by(Year, ADMIN2) %>% 
  summarise(Decision_TOTAL = length(Endemicity)) %>% 
  ungroup() %>% 
  # view() %>% 
  glimpse()

G_BF_IU_2017_Decision <- G_BF_IU_2017 %>% 
  select(Year, ADMIN1, ADMIN2, Endemicity, MDA_scheme) %>% 
  group_by(Year, ADMIN1, ADMIN2, Endemicity) %>% 
  summarise(Decision_count = length(Endemicity)) %>% 
  ungroup() %>% 
  # view() %>% 
  glimpse()

# Make a new df about summary of decisions (percent)
G_BF_IU_2017_Decision_Percent <- right_join(G_BF_IU_2017_Decision, G_BF_IU_2017_TotalDecision, by = c("Year", "ADMIN2"),) %>% 
  mutate(Decision_Percent = Decision_count/Decision_TOTAL) %>% 
  mutate(
    ADMIN2 = case_when(
      ADMIN2 == 'CENTRE-EST' ~ 'Center-East',
      ADMIN2 == 'HAUTS BASSINS' ~ 'Upper Basins',
      ADMIN2 == 'NORD' ~ 'North',
      ADMIN2 == 'CENTRE-NORD' ~ 'Center-North',
      ADMIN2 == 'CENTRE' ~ 'Center',
      ADMIN2 == 'BOUCLE DU MOUHOUN' ~ 'Mouhoun Loop',
      ADMIN2 == 'EST' ~ 'East',
      ADMIN2 == 'CENTRE-SUD' ~ 'Center-South',
      ADMIN2 == 'CASCADES' ~ 'Cascades',
      ADMIN2 == 'SUD-OUEST' ~ 'South-West',
      ADMIN2 == 'CENTRE- OUEST' ~ 'Center-West',
      ADMIN2 == 'PLATEAU CENTRAL' ~ 'Central Plateau',
      ADMIN2 == 'SAHEL' ~ 'Sahel',
      TRUE ~ ADMIN2
    )) %>% 
  filter(Endemicity != "Endemic (under post-intervention surveillance)",
         Year == 2021) %>% # for table, it is pretty clear that in 2021 there were regions wint not delivered MDA
  # view() %>% 
  glimpse()

ggplot(G_BF_IU_2017_Decision_Percent, aes(x = Year, y = Decision_Percent, fill = Endemicity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Endemicity and decision in each region", fill = "Region") +
  scale_fill_manual(values = c("darkred","lightgreen","steelblue","grey")) + #if there is null use "grey" for null
  theme_minimal() +
  facet_wrap(~ADMIN2) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

# 2.1. Year range (By using Year_start)
# To analyse, just change 'Year_start' to 1965,2000,2005,2010,2015
G_BF_ESPEN_Year <- G_BF_ESPEN %>% 
  select(Source,ADMIN1_NAME,Year,Prevalence) %>% 
  # filter(!is.na(Year_start) & Prevalence != 'null') %>%   # ALL Year
  filter(Year >= 2010 & Prevalence != 'null') %>%
  filter(ADMIN1_NAME == c("Centre", "Centre-Est", "Est", "Sud-Ouest")) %>% 
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  filter(Prevalence>=0.0001) %>% #Focused on prevalence >=0.0001 (WHO cutoff = 0.01)
  # view() %>% 
  glimpse()

# glimpse(G_BF_ESPEN_Year_start)

G_BF_ESPEN_Year_plot <- ggplot(data = G_BF_ESPEN_Year,
       aes(x = Year, y = Prevalence, fill = ADMIN1_NAME))+
  geom_col(position = 'dodge')
# facet_wrap(~ADMIN1_NAME, ncol = 1, scales = 'free')
G_BF_ESPEN_Year_plot

# How about boxplot???
G_BF_ESPEN_Year_boxplot <- ggplot(data = G_BF_ESPEN_Year,
                                  aes(x = factor(Year), y = Prevalence,
                                      fill = 0
                                      )) +
  geom_boxplot(color = "steelblue") +
  facet_wrap(~ ADMIN1_NAME, scales = "free_x", ncol = 2) +
  labs(x = "Year", y = "Prevalence", title = "Prevalence Boxplot by Year and Region") +
  theme_minimal()

G_BF_ESPEN_Year_boxplot

# Nevermind.ggplot is kinda annoying.

# FOCUSED ON SOUTH-WEST REGION (SUD-OUEST) #####################################
# What I wanna do is isolate the data from 2015-2023 in South-West, create a map
# How many IUs screened from 2015-2023?
South_west_plot <- G_BF_ESPEN_Y %>% 
  select(Method_1, Method_2, Examined, Year, ADMIN1_NAME, ADMIN2_NAME, IU_NAME, Latitude, Longitude, Prevalence) %>% 
  filter(Method_1 != "Clinical",
         !is.na(Prevalence),
         ADMIN1_NAME == "Sud-Ouest") %>% 
  mutate(
    ADMIN1_NAME = case_when(
      ADMIN1_NAME == 'Sud-Ouest' ~ 'South-West',
      TRUE ~ ADMIN1_NAME
    )) %>% 
  mutate(Prevalence = as.numeric(Prevalence),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  group_by(Year, ADMIN1_NAME, ADMIN2_NAME) %>% 
  summarise(Prevalence = mean(Prevalence),
            Latitude = mean(Latitude),
            Longitude = mean(Longitude)) %>% 
  ungroup() %>% 
  # view() %>% 
  glimpse()

# Plot
ggplot(South_west_plot, aes(x = Year, y = Prevalence)) +
  geom_col(fill = "lightblue", color = "black") + # Add black outline to bars
  facet_wrap(~ADMIN2_NAME, scales = "free_y") +
  labs(x = "Year", y = "Prevalence (%)", fill = "") +
  scale_fill_manual(values = "lightblue") +
  ggtitle("The Prevalence of LF in South-West Region (2015-2021)") +
  theme_minimal() +  # Use theme_minimal for white background
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),  # Black border around panels
        plot.background = element_rect(fill = "white")) +  # White background
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  guides(fill = FALSE)  # Remove legend

################################################################################
# Now, isolate the geospatial information
# I feel like a dum-dum.
# Why the lat-long is not summarised above is because there is "null" in lat-long with available prevalence.
# Filtering will cause data loss.

# Update: I tried by using mean(Latitude) and mean(Longitude) and the points were not on the place they should be.
# SHOW all Prevalence instead:
# Maximum Prevalence for each ADMIN2_NAME
South_west_GPS <- G_BF_ESPEN_Y %>% 
  select(Method_1, Method_2, Examined, Year, ADMIN1_NAME, ADMIN2_NAME, IU_NAME, LocationName, Latitude, Longitude, Prevalence) %>% 
  filter(Method_1 != "Clinical",
         !is.na(Prevalence),
         ADMIN1_NAME == "Sud-Ouest") %>% 
  mutate(
    ADMIN1_NAME = case_when(
      ADMIN1_NAME == 'Sud-Ouest' ~ 'South-West',
      TRUE ~ ADMIN1_NAME),
      Prevalence = Prevalence/100, # G_BF_ESPEN calculated prev in % (line 233); change it to proportion & combine the data below
    ) %>% 
  # group_by(ADMIN2_NAME) %>%
  filter(!is.na(Latitude),
         Latitude != "null",
         Latitude <= 13, # I saw some inconsistencies with small prevalence (distance too far away)
         Longitude > -0.7 # GPS inconsistencies (distance too far from South-west region)
  #        Prevalence == max(Prevalence, na.rm = TRUE)
  ) %>%
  mutate(Prevalence = as.numeric(Prevalence),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>% 
  # ungroup() %>% 
  select(Year, ADMIN2_NAME, LocationName, Latitude, Longitude, Prevalence) %>% # Select THIS
  view() %>%
  glimpse()

# A function required coz' Sawadogo's paper use degr-mins-secs (DMS) -_-)
dt_dec <- function(degr, mins, secs, hems) {
  decimal_degr <- degr + mins / 60 + secs / 3600
  if (toupper(hems) == "W" | toupper(hems) == "S") {
    decimal_degr <- -decimal_degr
  }
  return(decimal_degr)
}

# Bapla (10°53'25.83"N;3°15'46.74"W)
lat_bapla <- dt_dec(10, 53, 25.83, "N")
lon_bapla <- dt_dec(3, 15, 46.74, "W")

# Ouessa (11°02'19.59"N;2°47'7.57"W)
lat_ouessa <- dt_dec(11, 2, 19.59, "N")
lon_ouessa <- dt_dec(2, 47, 7.57, "W")

# Koudjo (9°53'50.41"N; 2°58'43.43"W)
lat_koudjo <- dt_dec(9, 53, 50.41, "N")
lon_koudjo <- dt_dec(2, 58, 43.43, "W")

# New mosquito GPS df
# Recall isolated df
# See isoolated result:
G_BF_ESPEN_Y_isod <- G_BF_ESPEN_Y %>% 
  select(Year, ADMIN1_NAME, ADMIN2_NAME, LocationName, Latitude, Longitude, Positive, Examined, Prevalence, Calc_Prevalence, lo_CI, up_CI) %>% 
  filter(ADMIN1_NAME == "Sud-Ouest") %>% # Focused on South-west
  view() %>% 
  glimpse()

# From isolated results to compiled with mosquito data, OR calculate mosquito data manually???
Mosquitoes_GPS_all <- data.frame(
  Year = c(2016, 2016, 2016),
  ADMIN1_NAME = c("Sud-Ouest", "Sud-Ouest", "Sud-Ouest"),
  ADMIN2_NAME = c("Bapla", "Ouessa", "Koudjo"),
  LocationName = c("Bapla", "Ouessa", "Koudjo"),
  Latitude = c(lat_bapla, lat_ouessa, lat_koudjo),
  Longitude = c(lon_bapla, lon_ouessa, lon_koudjo),
  Positive = c(2, 2, NaN),
  Examined = c(594, 59, NaN),
  Prevalence = c(0.003367, 0.033898, NaN),
  Calc_Prevalence = c(0.003367, 0.033898, NaN)
) %>% 
  view() %>% 
  glimpse()

# I think we have to insert Koudjo after calculating CI -_-)
Mosquitoes_GPS <- Mosquitoes_GPS_all %>% 
  filter(ADMIN2_NAME != "Koudjo") %>% 
  mutate(Conf_Int = binom.exact(Positive, Examined)) %>% 
  mutate(lo_CI = Conf_Int$lower,
         up_CI = Conf_Int$upper) %>% # Show confint data
  view() %>% 
  glimpse()

Mosquitoes_isod <- Mosquitoes_GPS %>% 
  select(Year, ADMIN1_NAME, ADMIN2_NAME, LocationName, Latitude, Longitude, Positive, Examined, Prevalence, Calc_Prevalence, lo_CI, up_CI) %>%
  view() %>% 
  glimpse()

# Combined!
SouthWest_isod_GPS_ALL <- rbind(G_BF_ESPEN_Y_isod, Mosquitoes_isod) %>% 
  mutate(Prev_label = paste(round(Prevalence*100, 3), "%")) %>% 
  view() %>% 
  glimpse()

# Manually add Koudjo -_-)
KOUDJOOO <- Mosquitoes_GPS_all %>%
  filter(ADMIN2_NAME == "Koudjo") %>% 
  mutate(lo_CI = NaN,
         up_CI = NaN,
         Prev_label = NA) %>% 
  view()

SouthWest_isod_GPS_ALL <- rbind(SouthWest_isod_GPS_ALL, KOUDJOOO) %>% 
  view()

write.csv(SouthWest_isod_GPS_ALL, "data_Compiled_ESPEN_Mosquitoes_ALL.csv")
# CREATING xy Confidence Interval ##############################################
dat <- read.csv("data_Compiled_ESPEN_Mosquitoes_ALL.csv")
view(dat)

dat_mosquitoes <- dat %>% 
  select(Year, ADMIN2_NAME, LocationName, Prevalence, lo_CI, up_CI) %>% 
  filter(ADMIN2_NAME == "Bapla" | ADMIN2_NAME == "Ouessa" | ADMIN2_NAME == "Koudjo") %>% 
  rename(Prev_M = Prevalence,
         lo_CI_M = lo_CI,
         up_CI_M = up_CI) %>% 
  view()

dat_human <- dat %>% 
  select(Year, ADMIN2_NAME, LocationName, Prevalence, lo_CI, up_CI) %>% 
  filter(ADMIN2_NAME != "Bapla" & ADMIN2_NAME != "Ouessa" & ADMIN2_NAME != "Koudjo") %>% 
  filter(ADMIN2_NAME == "Dano" | ADMIN2_NAME == "Diebougou") %>% # coz Bapla near Diebougou & Ouessa near Dano
  rename(Prev_H = Prevalence,
         lo_CI_H = lo_CI,
         up_CI_H = up_CI) %>% 
  mutate(
    Nearby = case_when(
      ADMIN2_NAME == "Dano" ~ "Ouessa",
      ADMIN2_NAME == "Diebougou" ~ "Bapla",
      TRUE ~ NA_character_
    )
  ) %>% 
  # view() %>% 
  glimpse()

# Left join dat_human with dat_mosquitoes based on ADMIN2_NAME
dat_human <- dat_human %>%
  left_join(dat_mosquitoes, by = c("Nearby" = "ADMIN2_NAME")) %>%
  view()

write.csv(dat_human, "data_Compiled_ESPEN_Mosquitoes_xyConfInt.csv")

dat <- read.csv("data_Compiled_ESPEN_Mosquitoes_xyConfInt.csv")
dat_human <- dat %>%
  filter(Year.x >= 2016 & Year.x <= 2019) %>% 
  glimpse()

# PLOT!
# col_year <- c("orange", "yellow", "lightgreen", "darkgreen", "lightblue", "steelblue", "purple") # All years
col_year <- c("orange", "steelblue", "lightgreen", "darkgreen")

plot(dat_human$Prev_H, dat_human$Prev_M, type = "p",
     xlim = c(0, 0.1), ylim = c(0, 0.13),
     col = col_year[as.factor(dat_human$Year.x)],
     pch = 19,
     # alpha = 0.5,
     xlab = "Proportion of mf prevalence in human", ylab = "Proportion of positive mosquitoes")

# Horizontals & verticals
segments(dat_human$Prev_H, dat_human$lo_CI_M, dat_human$Prev_H, dat_human$up_CI_M,
         col = col_year[as.factor(dat_human$Year.x)])
segments(dat_human$lo_CI_H, dat_human$Prev_M, dat_human$up_CI_H, dat_human$Prev_M,
         col = col_year[as.factor(dat_human$Year.x)])

legend("topright", legend = unique(as.factor(dat_human$Year.x)),
       col = col_year[as.factor(unique(dat_human$Year.x))],
       bty = "n", pch =19, cex = 1)


# U have to load the InfHuman data #############################################
An_g <- read.csv("Output_InfHuman_Angambiae.csv")
An_a <- read.csv("Output_InfHuman_Anarabiensis.csv")

dat <- left_join(An_g, An_a, by = "InfHuman_values", relationship = "many-to-many")
# x = An. gambiae (red)
# y = An. arabiensis (blue)
dat <- dat %>% 
  filter(InfHuman_values <= .10)
lines(dat$InfHuman_values, dat$Pos_loop.x, # An. gambiae
      col = "red")
text(0.00471, 0.033898, "(Dano, Ouessa)", adj = c(-0.1, -0.5), col = "black")
text(0.061, 0.003367, "(Diebougou, Bapla)", adj = c(-0.1, -0.5), col = "black")
legend("topright", legend = c("Vector Model"),
       col = c("red"), lty = 1, cex = 1,
       bty = "n", inset = c(-0.1, 0.3))

# PLOT Mosquitoes Only!!! ######################################################
Mosquitoes_only <- SouthWest_isod_GPS_ALL %>% 
  filter(LocationName == "Bapla" | LocationName == "Ouessa") %>% 
  mutate(Year = as.numeric(round(Year,0)))

# Plot for only Mosquitoes with CI:
ggplot(Mosquitoes_only, aes(x = Year, y = Calc_Prevalence,
                            ymin = lo_CI, ymax = up_CI)) +
  geom_point(shape=21, fill="lightblue", color="black", size=3) +
  geom_hline(yintercept=.003, linetype="dashed", color = "red") + # 0.3% of infective An. gambiae from model (in proportion)
  geom_hline(yintercept=.006, linetype="dashed", color = "red") + # 0.6% of positive An. gambiae from model (in proportion)
  facet_wrap(~ LocationName) +
  labs(x = "Year", y = "Prevalence (in proportion)", title = "The prevalence of positive mosquitoes in South-west Region") +
  geom_segment(aes(x = Year, xend = Year, y = lo_CI, yend = up_CI), color = "darkred") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  scale_x_continuous(breaks = 2016)

# From LatLon to distance ######################################################
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert to radians
  lat1 <- lat1*pi/180
  lon1 <- lon1*pi/180
  lat2 <- lat2*pi/180
  lon2 <- lon2*pi/180
  
  # Earth radius (km)
  R <- 6371
  
  # Diffs
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Calculate distance
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  distance <- R * c
  
  return(distance)
}

# Calculate distances between each pair of locations (use LocationName instead of ADMIN2_NAME)
distances <- expand.grid(ADMIN2_NAME1 = SouthWest_isod_GPS_ALL$LocationName,
                         ADMIN2_NAME2 = SouthWest_isod_GPS_ALL$LocationName) %>%
  mutate(distance = haversine_distance(SouthWest_isod_GPS_ALL$Latitude[match(ADMIN2_NAME1, SouthWest_isod_GPS_ALL$LocationName)], 
                                       SouthWest_isod_GPS_ALL$Longitude[match(ADMIN2_NAME1, SouthWest_isod_GPS_ALL$LocationName)], 
                                       SouthWest_isod_GPS_ALL$Latitude[match(ADMIN2_NAME2, SouthWest_isod_GPS_ALL$LocationName)], 
                                       SouthWest_isod_GPS_ALL$Longitude[match(ADMIN2_NAME2, SouthWest_isod_GPS_ALL$LocationName)]))
distances_df <- data.frame(
  LocationName1 = distances$ADMIN2_NAME1,
  LocationName2 = distances$ADMIN2_NAME2,
  distance_km = distances$distance) %>% 
  filter(distance_km != 0) %>% 
  view() %>% 
  glimpse()



################################################################################
# INTERACTIVE MAP FOR PREVALENCE IN MOSQUITOES AND HUMAN
################################################################################
# By directly using leaflet coz now I have the data point???
# 3.1. for hovered label!
# spdf data to separate Burkina Faso data by regions:
BF_spdf_sf <- st_as_sf(BF_spdf, coords = c("longitude", "latitude"), crs = '4326')
# view(BF_spdf_sf)
glimpse(BF_spdf_sf)

library(leaflet)

# Add colours to mosquitoes as red
color <- ifelse(SouthWest_isod_GPS_ALL$LocationName %in% c("Bapla", "Ouessa", "Koudjo"), "red", "blue")

# Labeling the flags
Labell <- paste(
  'Region: ', SouthWest_isod_GPS_ALL$ADMIN2_NAME, '(', South_west_GPS_ALL$LocationName, ')','<br/>',
  'Year: ', SouthWest_isod_GPS_ALL$Year, '<br/>',
  'Prev: ', SouthWest_isod_GPS_ALL$Prev_label, '<br/>', 
  sep="") %>%
  lapply(htmltools::HTML)

# Create a leaflet map
map <- leaflet(BF_spdf_sf) %>%
  # Add tiles
  addTiles() %>% 
  setView( lat=12.3710, lng=-1.5197, zoom=7) %>%
  addPolygons( stroke = F,
               fillOpacity = 0.3,
               fillColor = "white") %>% 
  # Add markers for each point
  addCircleMarkers(
    ~South_west_GPS_ALL$Longitude, ~South_west_GPS_ALL$Latitude,
    label = Labell,
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep labels always visible
      direction = "bottom",
      textOnlyIfOverlapping = TRUE
    ),
    color = color,
    popup = ~paste("Latitude:", round(SouthWest_isod_GPS_ALL$Latitude, 6), "<br>",
                   "Longitude:", round(SouthWest_isod_GPS_ALL$Longitude, 6))
  ) %>% 
  # Add a tile layer for the world with white color
  addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 1, maxZoom = 18)) %>% 
  # Add a geojson layer for the world with white fill color
  addGeoJSON("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json",
             fillColor = "white", fillOpacity = 1, color = "white", weight = 1)

# Display the map
map

################################################################################
# 3. Data preparation for GADM map #############################################
glimpse(G_BF_ESPEN)
unique(G_BF_ESPEN$ADMIN2_NAME)
# Maybe data that I want to analyse:
# 1. Source
# 2. ADMIN1_NAME (13 regions)
# 3. ADMIN2_NAME (76 administrative, thingy)
# 4. Latitude
# 5. Longitude
# 6. PoT
# 7. Year
# 8. Examined
# 9. Positive
# 10. Prevalence

# Pseudocode:
# 1. select data, filter year to >=2015 & prevalence !='null'
# 2. group_by Region (ADMIN1_NAME), on max(Prevalence)
# 3. df should have 13 values consisting of each names with max prevalence in year >= 2015

G_BF_ESPEN_Admin1_forGADM <- G_BF_ESPEN %>% 
  select(Source,ADMIN1_NAME,ADMIN2_NAME,Year,Year_end,Latitude,Longitude,PoT,Examined,Positive,Prevalence) %>% 
  filter(Year >= 2017 & Prevalence!= 'null') %>% #Year _start >=2015 coz' prevalence can be 70% in 1960s
  # filter(ADMIN1_NAME ==c("Sud-Ouest","Centre-Est","Centre","Est")) %>% # 4 regions with prevalence >=0.04 in 2015 and beyond
  # filter(ADMIN2_NAME !="null") %>% 
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  # filter(Prevalence>=0.0001) %>% #Focused on prevalence >=0.0001
  group_by(ADMIN1_NAME) %>% 
  # filter(Prevalence == max(Prevalence, na.rm = TRUE)) %>%
  # summarise(Max_Prevalence = max(Prevalence, na.rm = TRUE)) %>%
  slice(which.max(Prevalence)) %>%
  ungroup() %>%
  mutate(Examined = as.integer(Examined)) %>% 
  mutate(Positive = as.integer(Positive)) %>% 
  # Why don't we add a label for prevalence? B)
  mutate(Prev_label = paste("(", as.character(Positive), "/", as.character(Examined),
                            "=", round(Prevalence * 100, 3), "%)")) %>% 
  # view() %>% 
  glimpse()


# 4. Combine G_BF_ESPEN_Admin1_forGADM to *.shp data (CANNOT BE RUN VICE-VERSA)!!!
# Convert the 'SpatialPolygonsDataFrame' to 'sf' object first:
# Recall BF_spdf <- st_read(dsn = BF_shp_path_LINUX)

BF_spdf_sf <- st_as_sf(BF_spdf, coords = c("longitude", "latitude"), crs = '4326')
# view(BF_spdf_sf)
glimpse(BF_spdf_sf)

Comm_merged <- merge(BF_spdf_sf, G_BF_ESPEN_Admin1_forGADM, by.x = 'NAME_1', by.y = 'ADMIN1_NAME', all.x = TRUE) %>% 
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  # view() %>% 
  glimpse()

################################################################################
# 3.1. LOAD interactive maps (LEAFLET), map1
################################################################################
# 3.1. for hovered label!

library(leaflet)

# Leaflet TRIAL!
mypalette <- colorNumeric(palette='Reds',
                          domain=c(Comm_merged$Prevalence),
                          na.color="lightblue")
mypalette(c(0.0001,10))

mytext1 <- paste(
  'Region: ', Comm_merged$NAME_1, '<br/>',
  Comm_merged$NAME_2, '(GADM)', Comm_merged$ADMIN2_NAME, '(ESPEN)', '<br/>', 
  'Year: ', Comm_merged$Year, '<br/>',
  'Desc: ', Comm_merged$Prev_label, '<br/>', 
  sep="") %>%
  lapply(htmltools::HTML)

map1 <- leaflet(Comm_merged) %>% 
  addTiles()  %>% 
  setView( lat=12.3710, lng=-1.5197, zoom=7) %>%
  addPolygons( stroke = F,
               # fillOpacity = 0.5,
               fillOpacity = 0.7,
               fillColor = ~mypalette(Prevalence),
               label = mytext1,
               labelOptions = labelOptions( 
                 style = list('font-weight' = 'normal', padding = '3px 8px'), 
                 textsize = '15px', 
                 direction = 'auto')) %>% 
  addLegend('bottomright', pal = mypalette, values = ~Prevalence,
            title = 'Prevalence',
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1)

map1



# 3.2. for permanent label (only for positive incidents)!
# U have to create new df ONLY for positive incidences (prevalence) linked to text
glimpse(Comm_merged)
# Coz' I'm a dum-dum, I tried to separate the df into 2 and then combine them

# Compiled geometry
Compiled_geom <- Comm_merged %>%
  # filter(Prevalence != 0) %>%
  group_by(NAME_1) %>%
  summarise(Prevalence = mean(Prevalence),
            geometry = st_union(geometry)) %>%
  ungroup() %>%
  arrange(NAME_1) %>% 
  view()

# The final df
Comm_merged_PosOnly <- Comm_merged %>% 
  # filter(Prevalence != 0) %>% 
  group_by(NAME_1) %>% 
  slice(which.max(Prevalence)) %>%
  ungroup() %>%
  select(-geometry) %>% 
  arrange(NAME_1) %>% 
  mutate(geometry = Compiled_geom$geometry) %>% 
  view()


mytext2 <- paste(
  Comm_merged_PosOnly$NAME_1, '<br/>', 
  Comm_merged_PosOnly$Year, '<br/>',
  Comm_merged_PosOnly$Prev_label, '<br/>', 
  sep="") %>%
  lapply(htmltools::HTML)

map2 <- leaflet(Comm_merged_PosOnly) %>% 
  addTiles()  %>% 
  setView( lat=12.3710, lng=-1.5197, zoom=7.5) %>%
  addPolygons( stroke = T, fillOpacity = 0.8,
               fillColor = ~mypalette(Prevalence),
               label = mytext2,
               labelOptions = labelOptions(noHide = T, textOnly = T,
                                           style = list('text-align' = 'center',
                                                        'font-weight' = 'normal'), 
                                           textsize = '12px', 
                                           direction = 'center')) %>% 
  addLegend('bottomright', pal = mypalette, values = ~Prevalence,
            title = 'Prevalence',
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1)

map2


# 3.3. focused on 2021-2022! ###################################################
# Kinda different thing, so I rearrange needed data:

# Pseudocode:
# 1. select data, filter year to 2022 & prevalence !='null'
# 2. group_by Region (ADMIN1_NAME), on max(Prevalence)
# 3. df should have 13 values consisting of each names with max prevalence in year >= 2020
G_BF_ESPEN_2022_forGADM <- G_BF_ESPEN %>% 
  select(Source,ADMIN1_NAME,ADMIN2_NAME,Year,Year_end,Latitude,Longitude,PoT,Examined,Positive,Prevalence) %>% 
  filter(Year == 2023 & Prevalence!= 'null') %>% #CHANGE YEAR into 2020, 2021, 2022, OR 2023!!!
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  # filter(Prevalence>=0.0001) %>% #Focused on prevalence >=0.0001
  # group_by(ADMIN1_NAME) %>% 
  # slice(which.max(Prevalence)) %>%
  # ungroup() %>%
  mutate(Examined = as.integer(Examined)) %>% 
  mutate(Positive = as.integer(Positive)) %>% 
  # Why don't we add a label for prevalence? B)
  mutate(Prev_label = paste("(", as.character(Positive), "/", as.character(Examined),
                            "=", round(Prevalence * 100, 3), "%)")) %>% 
  view()

chara_name1 <- factor(unique(G_BF_ESPEN_2022_forGADM$ADMIN1_NAME))
chara_name1

# Filter GADM data to NAME_1 in ESPEN_2022
BF_spdf_2022 <- BF_spdf %>% 
  filter(NAME_1 == chara_name1) %>% 
  view()

# Seems like it will be impossible to generate map based on NAME_2 or NAME_3#####
sort(unique(G_BF_ESPEN_2022_forGADM$ADMIN2_NAME))
sort(unique(BF_spdf_2022$NAME_2))
sort(unique(BF_spdf_2022$NAME_3))

# Compare names in ESPEN (ADMIN2_NAME) & GADM (NAME_2 OR NAME_3)
# ADMIN2_NAME vs. NAME_3 (NAME_2 FAILED)
Corrected_ESPEN_2022 <- G_BF_ESPEN_2022_forGADM[!G_BF_ESPEN_2022_forGADM$ADMIN2_NAME %in% BF_spdf_2022$NAME_3,]
unique(Corrected_ESPEN_2022$ADMIN2_NAME)

Corrected_GADM_2022 <- BF_spdf_2022[!BF_spdf_2022$NAME_3 %in% G_BF_ESPEN_2022_forGADM$ADMIN2_NAME,]
unique(Corrected_GADM_2022$NAME_3)
# Seems like it will be impossible to generate map based on NAME_2 or NAME_3#####
#####

# 4. Combine G_BF_ESPEN_Admin1_forGADM to *.shp data (CANNOT BE RUN VICE-VERSA)!!!
# Convert the 'SpatialPolygonsDataFrame' to 'sf' object first:
# Recall BF_spdf <- st_read(dsn = BF_shp_path_LINUX)

# Notes: DO NOT FIlter GADM Data!!!!
BF_spdf_sf <- st_as_sf(BF_spdf, coords = c("longitude", "latitude"), crs = '4326')
# view(BF_spdf_sf)
glimpse(BF_spdf_sf)

# GADM: Compile multipolygon into NAME_1:
Compiled_geom <- BF_spdf_sf %>%
  group_by(NAME_1) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  arrange(NAME_1) %>% 
  view()

# ESPEN: Compile prevalence into max:
G_BF_ESPEN_2022_forGADM_MaxPrev <- G_BF_ESPEN_2022_forGADM %>% 
  group_by(ADMIN1_NAME) %>% 
  slice(which.max(Prevalence)) %>%
  ungroup() %>%
  view()

# MERGE!
Comm_merged_2022 <- merge(Compiled_geom, G_BF_ESPEN_2022_forGADM_MaxPrev, by.x = 'NAME_1', by.y = 'ADMIN1_NAME', all.x = TRUE) %>% 
  mutate(Prevalence = as.numeric(Prevalence)) %>% 
  filter(!is.na(Prevalence)) %>% 
  view() %>% 
  glimpse()

# Comm_merged_2022 <- sf::st_as_sf(Comm_merged_2022)

library(leaflet)

# Leaflet TRIAL!
mypalette <- colorNumeric(palette='Reds',
                          domain=c(Comm_merged$Prevalence),
                          na.color="Green")
mypalette(c(0.0001,10))
mytext3 <- paste(
  Comm_merged_2022$NAME_1, ' (', Comm_merged_2022$ADMIN2_NAME, ') ', '<br/>', 
  Comm_merged_2022$Year, '<br/>',
  Comm_merged_2022$Prev_label, '<br/>', 
  sep="") %>%
  lapply(htmltools::HTML)

map3 <- leaflet(Comm_merged_2022) %>% 
  addTiles()  %>% 
  setView( lat=12.3710, lng=-1.5197, zoom=7.5) %>%
  addPolygons( stroke = T, fillOpacity = 0.8,
               fillColor = ~mypalette(Prevalence),
               label = mytext3,
               labelOptions = labelOptions(noHide = T, textOnly = T,
                                           style = list('text-align' = 'center',
                                                        'font-weight' = 'normal'), 
                                           textsize = '12px', 
                                           direction = 'center')) %>% 
  addLegend('bottomright', pal = mypalette, values = ~Prevalence,
            title = 'Prevalence',
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1)

map3
