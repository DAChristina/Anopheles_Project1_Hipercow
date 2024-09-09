# The proportion of mosquitoes with established infection

# Contents: ####################################################################
# 1. Data preparation
# 2. Visualisation

# 1. Data preparation ##########################################################
wd_R_mosquitoes_Reference = "/home/ron/Downloads/2023 Imperial MRes Journey/2023 Project1 Weekly Meeting"
setwd(wd_R_mosquitoes_Reference)

library(tidyverse)
library(readxl)

dat <- read_excel('Mosquito Mathematical Models_15.01.2024_DC.xlsx') %>% 
  # view() %>% 
  glimpse()

# Analyse min-max blood samples ################################################
dat_Report <- dat %>% 
  select(Reference) %>% 
  filter(!duplicated(Reference)) %>%
  view()


dat_Report <- dat %>% 
  mutate(Reference = ifelse(Reference == "(Bryan & Southgate, 1988a), (Bryan & Southgate, 1988b)", "(Bryan & Southgate, 1988a)", Reference)) %>% 
  select(Reference, Blood_sampling_technique, Human_mf_intensity_otherunits, Human_mf_intensity_per1mL, Human_mf_1mL_to_20uL_standardization_notes, Human_mf_intensity_per20uL) %>% 
  glimpse()

# No. of human mf densities studied
dat_human_mfCount <- dat %>% 
  mutate(Reference = ifelse(Reference == "(Bryan & Southgate, 1988a), (Bryan & Southgate, 1988b)", "(Bryan & Southgate, 1988a)", Reference)) %>% 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>%
  group_by(Reference, Mosquito_species) %>% 
  count() %>%
  rename("No. of human mf densities studied" = n) %>% 
  view()

# Range of Human mf densities --> range(min-max)
dat_human_mfRange_20uL <- dat %>% 
  mutate(Reference = ifelse(Reference == "(Bryan & Southgate, 1988a), (Bryan & Southgate, 1988b)", "(Bryan & Southgate, 1988a)", Reference)) %>% 
  mutate(Human_mf_intensity_per1mL = as.numeric(Human_mf_intensity_per1mL)) %>% 
  mutate(Human_mf_intensity_per20uL = as.numeric(Human_mf_intensity_per20uL)) %>% 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>%
  group_by(Reference, Mosquito_species) %>% 
  mutate(min_mfH_20uL = min(Human_mf_intensity_per20uL),
         max_mfH_20uL = max(Human_mf_intensity_per20uL)) %>% 
  distinct(Reference, Mosquito_species, .keep_all = TRUE) %>% 
  glimpse()

dat_human_mfRange_1mL <- dat %>% 
  mutate(Reference = ifelse(Reference == "(Bryan & Southgate, 1988a), (Bryan & Southgate, 1988b)", "(Bryan & Southgate, 1988a)", Reference)) %>% 
  mutate(Human_mf_intensity_per1mL = as.numeric(Human_mf_intensity_per1mL)) %>% 
  mutate(Human_mf_intensity_per20uL = as.numeric(Human_mf_intensity_per20uL)) %>% 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>%
  group_by(Reference, Mosquito_species) %>% 
  mutate(min_mfH_1mL = min(Human_mf_intensity_per1mL),
         max_mfH_1mL = max(Human_mf_intensity_per1mL)) %>% 
  distinct(Reference, Mosquito_species, .keep_all = TRUE) %>% 
  glimpse()

dat_human_mfRange_combined <- left_join(dat_human_mfRange_20uL, dat_human_mfRange_1mL, 
                                        by = c("Reference", "Mosquito_species")) %>% 
  glimpse()

dat_human_mfRange_combined <- dat_human_mfRange_combined %>% 
  select(Reference, Mosquito_species, min_mfH_20uL, max_mfH_20uL, min_mfH_1mL, max_mfH_1mL) %>% 
  view()

# No. of mosquitoes dissected per mf density range(min-max)
dat_mosquito_dissected <- dat %>% 
  mutate(Reference = ifelse(Reference == "(Bryan & Southgate, 1988a), (Bryan & Southgate, 1988b)", "(Bryan & Southgate, 1988a)", Reference)) %>% 
  mutate(Mosquito_totaldissected = as.numeric(Mosquito_totaldissected)) %>% 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>% 
  filter(grepl("^[0-9.]+$", Mosquito_totaldissected)) %>%
  group_by(Reference, Mosquito_species) %>% 
  summarise(min_dissected = min(Mosquito_totaldissected),
            max_dissected = max(Mosquito_totaldissected)) %>% 
  # distinct(Reference, Mosquito_species, .keep_all = TRUE) %>% 
  view()

# Mean and range of mfE per-reference per-species
dat_mean_mfE <- dat %>% 
  mutate(Reference = ifelse(Reference == "(Bryan & Southgate, 1988a), (Bryan & Southgate, 1988b)", "(Bryan & Southgate, 1988a)", Reference)) %>% 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>% 
  filter(grepl("^[0-9.]+$", mf_mean_arithmetic_pertotaldissectedmosquito)) %>%
  mutate(mf_mean_arithmetic_pertotaldissectedmosquito = as.numeric(mf_mean_arithmetic_pertotaldissectedmosquito)) %>% 
  group_by(Reference, Mosquito_species) %>% 
  summarise(mean_mfE = mean(mf_mean_arithmetic_pertotaldissectedmosquito),
            min_mfE = min(mf_mean_arithmetic_pertotaldissectedmosquito),
            max_mfE = max(mf_mean_arithmetic_pertotaldissectedmosquito)) %>% 
  distinct(Reference, Mosquito_species, .keep_all = TRUE) %>% 
  view()


# Mean and range of established larvae per-reference per-species
dat_mean_Larvae <- dat %>% 
  mutate(Reference = ifelse(Reference == "(Bryan & Southgate, 1988a), (Bryan & Southgate, 1988b)", "(Bryan & Southgate, 1988a)", Reference)) %>% 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>% 
  filter(grepl("^[0-9.]+$", Larvae_mean_arithmetic_perdissectedmosquito)) %>%
  mutate(Larvae_mean_arithmetic_perdissectedmosquito = as.numeric(Larvae_mean_arithmetic_perdissectedmosquito)) %>% 
  group_by(Reference, Mosquito_species) %>% 
  summarise(mean_Larv = mean(Larvae_mean_arithmetic_perdissectedmosquito),
            min_Larv = min(Larvae_mean_arithmetic_perdissectedmosquito),
            max_Larv = max(Larvae_mean_arithmetic_perdissectedmosquito)) %>% 
  distinct(Reference, Mosquito_species, .keep_all = TRUE) %>% 
  view()


# Selected data for analysis ###################################################
dat_sel <- dat %>% 
  select(Reference, Human_mf_intensity_per1mL, Human_mf_intensity_per20uL, Mosquito_species, Mosquito_totaldissected, Mosquito_mf_infected_count, Mosquito_mf_infected_proportion, mf_mean_arithmetic_pertotaldissectedmosquito, Mosquito_larvae_infected_count, Mosquito_larvae_infected_proportion_fromtotaldissected, Larvae_mean_arithmetic_perdissectedmosquito) %>% 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL)) %>% # Filter out info (e.g. "Captured mosquitoes," etc.)
  filter(grepl("^[0-9.]+$", Mosquito_totaldissected)) %>% # So used data based on mfH & total dissected of mosquitoes
  filter(Mosquito_species != "An. melas") %>% # Omit An. melas
  mutate(Human_mf_intensity_per1mL = as.numeric(Human_mf_intensity_per1mL)) %>% 
  mutate(Human_mf_intensity_per20uL = as.numeric(Human_mf_intensity_per20uL)) %>% 
  mutate(Mosquito_totaldissected = as.numeric(Mosquito_totaldissected)) %>% 
  mutate(Mosquito_mf_infected_count = as.numeric(Mosquito_mf_infected_count)) %>% 
  mutate(Mosquito_mf_infected_proportion = as.numeric(Mosquito_mf_infected_proportion)) %>% 
  mutate(mf_mean_arithmetic_pertotaldissectedmosquito = as.numeric(mf_mean_arithmetic_pertotaldissectedmosquito)) %>% 
  mutate(Mosquito_larvae_infected_count = as.numeric(Mosquito_larvae_infected_count)) %>% 
  mutate(Mosquito_larvae_infected_proportion_fromtotaldissected = as.numeric(Mosquito_larvae_infected_proportion_fromtotaldissected)) %>% 
  mutate(Larvae_mean_arithmetic_perdissectedmosquito = as.numeric(Larvae_mean_arithmetic_perdissectedmosquito)) %>% 
  
  # Calculations based on Snow et al (2002 & 2006)
  mutate(Human_mf20uL_log_calculated = .1149*(log10(Human_mf_intensity_per1mL+1))^2 + .037*(log10(Human_mf_intensity_per1mL+1)) - .0309) %>% 
  mutate(Human_mf20uL_calculated = 10^Human_mf20uL_log_calculated) %>% 
  mutate(mfE_log_mean_calculated = log10(-.0061+1)+.4010*log10(Human_mf_intensity_per20uL+1)) %>% # log10(mfE(mfH)) = log10(-0.0061+1)+0.4010*log10(mfH+1)
  mutate(mfE_mean_calculated = 10^mfE_log_mean_calculated) %>%
  mutate(Larvae_mean_calculated = (.03*mfE_mean_calculated^2.3520)/(1+((mfE_mean_calculated^2.3520)/482.5680))) %>% 
  # mutate(MosquitoInfected_calculated = ) # Analyse this part by using MLE
  glimpse()


# Additional report for calculated mfE for mosquitoes with established infection
# I regret creating 3 variations of BryanSouthgate data, have to see & filter out the data again and again -_-)
dat_sel_BryanSouthgate <- dat %>% 
  filter(grepl("Bryan & Southgate", Reference)) %>% 
  select(Reference, Mosquito_species,
         Mosquito_mf_infected_FILTER, Mosquito_mf_infected_proportion, 
         Mosquito_larvae_infective_FILTER, Mosquito_larvae_infected_proportion_fromtotaldissected) %>% 
  view() %>% 
  glimpse()


dat_mfE_calc <- dat_sel %>% # MUTATE below 
  filter(grepl("^[0-9.]+$", Human_mf_intensity_per20uL),
         !is.na(Mosquito_larvae_infected_proportion_fromtotaldissected)) %>%
  group_by(Reference, Mosquito_species) %>% 
  mutate(mean_mfE = mean(mfE_mean_calculated),
         min_mfE = min(mfE_mean_calculated),
         max_mfE = max(mfE_mean_calculated),
         Larvae_mean = mean(Larvae_mean_calculated),
         mean_Mosquito_larvae_infected_proportion = mean(Mosquito_larvae_infected_proportion_fromtotaldissected),
         min_MosqInfected = min(Mosquito_larvae_infected_proportion_fromtotaldissected),
         max_MosqInfected = max(Mosquito_larvae_infected_proportion_fromtotaldissected)) %>% 
  distinct(Reference, Mosquito_species, .keep_all = TRUE) %>% 
  select(Reference, Mosquito_species, mean_mfE, min_mfE, max_mfE, Larvae_mean, mean_Mosquito_larvae_infected_proportion, min_MosqInfected, max_MosqInfected) %>% 
  view() %>% 
  glimpse()

dat_mfE_report <- dat_mfE_calc %>% 
  mutate(MeanRange_mfE = paste0(round(mean_mfE, 2), " (", round(min_mfE, 2), "-", round(max_mfE, 2), ") "),
         MeanRange_Mosq = paste0(round(mean_Mosquito_larvae_infected_proportion, 2), " (", round(min_MosqInfected, 2), "-", round(max_MosqInfected, 2), ") ")) %>% 
  select(Reference, Mosquito_species, MeanRange_mfE, MeanRange_Mosq, Larvae_mean) %>% 
  view() %>% 
  glimpse()

# 2. Visualisation #############################################################
# Boxplot of mfH mfE and Established Larvae, ALL REFERENCES
dat_LF <- dat %>% 
  filter(Mosquito_species != "An. melas") %>% # Omit An. melas
  select(Mosquito_species, Human_mf_intensity_per20uL, mf_mean_arithmetic_pertotaldissectedmosquito, Larvae_mean_arithmetic_perdissectedmosquito) %>% 
  mutate(Human_mf_intensity_per20uL = as.numeric(Human_mf_intensity_per20uL)) %>% 
  mutate(mf_mean_arithmetic_pertotaldissectedmosquito = as.numeric(mf_mean_arithmetic_pertotaldissectedmosquito)) %>% 
  mutate(Larvae_mean_arithmetic_perdissectedmosquito = as.numeric(Larvae_mean_arithmetic_perdissectedmosquito)) %>% 
  # view() %>% 
  glimpse()

mfH_counts <- dat_LF$Human_mf_intensity_per20uL
mfE_counts <- dat_LF$mf_mean_arithmetic_pertotaldissectedmosquito
Larvae_counts <- dat_LF$Larvae_mean_arithmetic_perdissectedmosquito

# Combine the counts into a list
counts_mfH <- list(mfH = mfH_counts)
counts_mfELarvae <- list(mfE = mfE_counts, Larvae = Larvae_counts)

# Calculate average points
mfH_ave <- mean(na.omit(mfH_counts))
mfE_ave <- mean(na.omit(mfE_counts))
Larvae_ave <- mean(na.omit(Larvae_counts))

# The Boxplot
par(mfrow = c(2,1), mar = c(4,4,2,2))
boxplot(mfH_counts, horizontal = TRUE, col = "lightblue",
        xlab = "Human mf count in 20 uL", ylab = "Human mf"
)
points(mfH_ave, 1, pch = 5, col = "black", cex = 1)

boxplot(Larvae_counts, mfE_counts, horizontal = TRUE, col = "lightblue",
        xlab = "Mean and distribution of ingested mf and established larvae per dissected mosquito", ylab = " ",
        names = c("Larvae", "Ingested mf")
)
points(Larvae_ave, 1, pch = 5, col = "black", cex = 1)
points(mfE_ave, 2, pch = 5, col = "black", cex = 1)
par(mfrow = c(1,1))



# Boxplot of mfH mfE and Established Larvae, OMIT BRENGUES AND COZ
dat_LF <- dat %>% 
  filter(Reference != "(Brengues & Coz, 1972)") %>% # Omit Brengues & Coz
  filter(Mosquito_species != "An. melas") %>% # Omit An. melas
  select(Mosquito_species, Human_mf_intensity_per20uL, mf_mean_arithmetic_pertotaldissectedmosquito, Larvae_mean_arithmetic_perdissectedmosquito) %>% 
  mutate(Human_mf_intensity_per20uL = as.numeric(Human_mf_intensity_per20uL)) %>% 
  mutate(mf_mean_arithmetic_pertotaldissectedmosquito = as.numeric(mf_mean_arithmetic_pertotaldissectedmosquito)) %>% 
  mutate(Larvae_mean_arithmetic_perdissectedmosquito = as.numeric(Larvae_mean_arithmetic_perdissectedmosquito)) %>% 
  # view() %>% 
  glimpse()

mfH_counts <- dat_LF$Human_mf_intensity_per20uL
mfE_counts <- dat_LF$mf_mean_arithmetic_pertotaldissectedmosquito
Larvae_counts <- dat_LF$Larvae_mean_arithmetic_perdissectedmosquito

# Combine the counts into a list
counts_mfH <- list(mfH = mfH_counts)
counts_mfELarvae <- list(mfE = mfE_counts, Larvae = Larvae_counts)

# Calculate average points
mfH_ave <- mean(na.omit(mfH_counts))
mfE_ave <- mean(na.omit(mfE_counts))
Larvae_ave <- mean(na.omit(Larvae_counts))

# The Boxplot
par(mfrow = c(2,1), mar = c(4,4,2,2))
boxplot(mfH_counts, horizontal = TRUE, col = "lightblue",
        xlab = "Human mf count in 20 uL", ylab = "Human mf"
)
points(mfH_ave, 1, pch = 5, col = "black", cex = 1)

boxplot(Larvae_counts, mfE_counts, horizontal = TRUE, col = "lightblue",
        xlab = "Mean and distribution of ingested mf and established larvae per dissected mosquito", ylab = " ",
        names = c("Larvae", "Ingested mf")
)
points(Larvae_ave, 1, pch = 5, col = "black", cex = 1)
points(mfE_ave, 2, pch = 5, col = "black", cex = 1)
par(mfrow = c(1,1))


# Additional plot mfH/mL vs. mfH/20uL
palette <- c("An. gambiae" = "red", "An. arabiensis" = "blue")

par(mfrow = c(1,1))
plot(dat_sel$Human_mf_intensity_per1mL, dat_sel$Human_mf_intensity_per20uL)
plot(dat_sel$Human_mf_intensity_per1mL, log10(dat_sel$Human_mf_intensity_per20uL+1))
points(dat_sel$Human_mf_intensity_per1mL+1, log10(dat_sel$Human_mf_intensity_per20uL+1), col = "red", pch = "-")

plot(log10(dat_sel$Human_mf_intensity_per1mL+1), log10(dat_sel$Human_mf_intensity_per20uL+1),
     xlab = "log10(Human mf count per 1 mL +1)",
     ylab = "log10(Human mf count per 20 uL +1)",)
points(log10(dat_sel$Human_mf_intensity_per1mL+1), dat_sel$Human_mf20uL_log_calculated, col = "black", pch = "-")

par(mfrow = c(1,2))
# mfH --> mfE, log-log plot
plot(log10(dat_sel$Human_mf_intensity_per20uL+1), log10(dat_sel$mf_mean_arithmetic_pertotaldissectedmosquito+1),
     cex = dat_sel$Mosquito_totaldissected/20,
     xlab = "log10(Human mf count per 20 uL +1)",
     ylab = "log10(Mean of ingested mf per dissected mosquito +1)",
     col = palette[as.character(dat_sel$Mosquito_species)])
points(log10(dat_sel$Human_mf_intensity_per20uL+1), dat_sel$mfE_log_mean_calculated, col = "black", pch = "-")

# mfH --> mfE, real data plot
plot(dat_sel$Human_mf_intensity_per20uL, dat_sel$mf_mean_arithmetic_pertotaldissectedmosquito,
     cex = dat_sel$Mosquito_totaldissected/20,
     xlab = "Human mf count per 20 uL",
     ylab = "Mean of ingested mf per dissected mosquito",
     col = palette[as.character(dat_sel$Mosquito_species)])
points(dat_sel$Human_mf_intensity_per20uL, dat_sel$mfE_mean_calculated, col = "black", pch = "-")

par(mfrow = c(1,1))

par(mfrow = c(1,2))
# mfE --> Larvae, log-log plot
plot(log10(dat_sel$mfE_mean_calculated+1), log10(dat_sel$Larvae_mean_arithmetic_perdissectedmosquito+1),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "log10(Mean of ingested mf per dissected mosquito +1)",
     ylab = "log10(Mean of larvae per dissected mosquito +1)",
     col = palette[as.character(dat_sel$Mosquito_species)])
points(log10(dat_sel$mfE_mean_calculated+1), log10(dat_sel$Larvae_mean_calculated+1), col = "black", pch = "-")

# mfE --> Larvae, real data plot
plot(dat_sel$mfE_mean_calculated, dat_sel$Larvae_mean_arithmetic_perdissectedmosquito,
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Mean of ingested mf per dissected mosquito",
     ylab = "Mean of larvae per dissected mosquito",
     col = palette[as.character(dat_sel$Mosquito_species)])
points(dat_sel$mfE_mean_calculated, dat_sel$Larvae_mean_calculated, col = "black", pch = "-")

par(mfrow = c(1,1))



# Plots expected to be estimated ###############################################
par(mfrow = c(3,1), mar = c(4,4,2,2)) # margin (bottom,left,top,right)
# mfH --> InfectedMosquitoes(mfH)
plot(dat_sel$Human_mf_intensity_per20uL, (dat_sel$Mosquito_larvae_infected_count/dat_sel$Mosquito_totaldissected),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Human mf count per 20 uL",
     ylab = "Proportion of mosquitoes with established infection",
     col = palette[as.character(dat_sel$Mosquito_species)])

# mfE --> InfectedMosquitoes(mfE)
plot(dat_sel$mfE_mean_calculated, (dat_sel$Mosquito_larvae_infected_count/dat_sel$Mosquito_totaldissected),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Mean of ingested mf per dissected mosquito",
     ylab = "Proportion of mosquitoes with established infection",
     col = palette[as.character(dat_sel$Mosquito_species)])

# Larvae --> InfectedMosquitoes(Larvae)
plot(dat_sel$Larvae_mean_calculated, (dat_sel$Mosquito_larvae_infected_count/dat_sel$Mosquito_totaldissected),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Mean of larvae per dissected mosquito",
     ylab = "Proportion of mosquitoes with established infection",
     col = palette[as.character(dat_sel$Mosquito_species)])

par(mfrow = c(1,1))


# Some basic stats #############################################################
dat_anal <- dat_sel %>% 
  select(Reference, Mosquito_species, Mosquito_larvae_infected_proportion_fromtotaldissected, Mosquito_totaldissected, Mosquito_larvae_infected_count, mfE_mean_calculated) %>% 
  filter(!is.na(Mosquito_larvae_infected_proportion_fromtotaldissected) & Mosquito_larvae_infected_proportion_fromtotaldissected != "not_analysed",
         !is.na(mfE_mean_calculated) & mfE_mean_calculated != "not_analysed") %>%
  mutate(Unique_ID = row_number()) %>% 
  # view() %>% 
  glimpse()

# To simplify my life a bit
y <- dat_anal$Mosquito_larvae_infected_proportion_fromtotaldissected
x <- dat_anal$mfE_mean_calculated
AllMosq <- dat_anal$Mosquito_totaldissected

summary(dat_anal)

dat_anal_numb <- dat_anal %>% 
  select(Mosquito_larvae_infected_proportion_fromtotaldissected, Mosquito_totaldissected, Mosquito_larvae_infected_count, mfE_mean_calculated) %>% 
  # view() %>% 
  glimpse()

cor(dat_anal_numb)
pairs(dat_anal_numb)


hist(x, main = "Histogram of expected average of ingested mfE", xlab = "Expected average of ingested mfE")
hist(y, main = "Histogram of mosquito proportion with established infection", xlab = "Mosquito proportion with established infection")
hist(AllMosq, main = "Histogram of total dissected mosquitoes", xlab = "Total dissected mosquitoes")


qqnorm(x)
qqline(x)

qqnorm(y)
qqline(y)

shapiro.test(x) # Not normal
shapiro.test(y) # Not normal

# Density plot???
plot(density(x), main = "Density Plot of Response Variable")
plot(density(y), main = "Density Plot of Predictor Variable")

dat_analSummary <- dat_anal %>% 
  mutate(mean_mfE = mean(mfE_mean_calculated),
         var_mfE = var(mfE_mean_calculated),
         PearsonChiSq_mfE = (var_mfE-mean_mfE)/mean_mfE, # mfE shows overdispersion
         mean_Mosq = mean(Mosquito_larvae_infected_proportion_fromtotaldissected),
         var_Mosq = var(Mosquito_larvae_infected_proportion_fromtotaldissected),
         PearsonChiSq_Mosq = (var_Mosq-mean_Mosq)/mean_Mosq) %>% # Mosq shows underdispersion
  view() %>% 
  glimpse()
