library(tidyverse)

dat_sel <- read.csv("inputs/Mosquito_EstablishedInfection_Data_clean.csv")

mfH_counts <- dat_sel$Human_mf_intensity_per20uL
mfE_counts <- dat_sel$mf_mean_arithmetic_pertotaldissectedmosquito
Larvae_counts <- dat_sel$Larvae_mean_arithmetic_perdissectedmosquito

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
palette <- c("An. gambiae" = "red", "An. arabiensis" = "blue", "An. melas" = "green")

par(mfrow = c(1,1))
plot(dat_sel$Human_mf_intensity_per1mL, dat_sel$Human_mf_intensity_per20uL)
plot(dat_sel$Human_mf_intensity_per1mL, log10(dat_sel$Human_mf_intensity_per20uL+1))
curve((.1149*(log10(x+1))^2 + .037*(log10(x+1)) - .0309), col = "red", add = TRUE)
points(dat_sel$Human_mf_intensity_per1mL+1, log10(dat_sel$Human_mf_intensity_per20uL+1), col = "red", pch = "-")

plot(log10(dat_sel$Human_mf_intensity_per1mL+1), log10(dat_sel$Human_mf_intensity_per20uL+1),
     xlab = "log10(Human mf count per 1 mL +1)",
     ylab = "log10(Human mf count per 20 uL +1)",)
# curve(log10(.1149*(log10(x+1))^2 + .037*(log10(x+1)) - .0309), col = "red", add = TRUE)
curve((.1149*((x+1))^2 + .037*((x+1)) - .0309), col = "red", add = TRUE)
points(log10(dat_sel$Human_mf_intensity_per1mL+1), dat_sel$Human_mf20uL_log_calculated, col = "black", pch = "-")

par(mfrow = c(1,2))
# mfH --> mfE, log-log plot
plot(log10(dat_sel$Human_mf_intensity_per20uL+1), log10(dat_sel$mf_mean_arithmetic_pertotaldissectedmosquito+1),
     cex = dat_sel$Mosquito_totaldissected/20,
     xlab = "log10(Human mf count per 20 uL +1)",
     ylab = "log10(Mean of ingested mf per dissected mosquito +1)",
     col = palette[as.character(dat_sel$Mosquito_species)])
curve((log10(-.0061+1)+.4010*log10(x+1)), col = "black", add = TRUE)
# points(log10(dat_sel$Human_mf_intensity_per20uL+1), dat_sel$mfE_log_mean_calculated, col = "black", pch = "-")

# mfH --> mfE, real data plot
plot(dat_sel$Human_mf_intensity_per20uL, dat_sel$mf_mean_arithmetic_pertotaldissectedmosquito,
     cex = dat_sel$Mosquito_totaldissected/20,
     xlab = "Human mf count per 20 uL",
     ylab = "Mean of ingested mf per dissected mosquito",
     col = palette[as.character(dat_sel$Mosquito_species)])
curve(10^(log10(-.0061+1)+.4010*log10(x+1)), col = "black", add = TRUE)
# points(dat_sel$Human_mf_intensity_per20uL, dat_sel$mfE_mean_calculated, col = "black", pch = "-")

par(mfrow = c(1,1))

par(mfrow = c(1,2))
# mfE --> Larvae, log-log plot
plot(log10(dat_sel$mfE_mean_calculated+1), log10(dat_sel$Larvae_mean_arithmetic_perdissectedmosquito+1),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "log10(Mean of ingested mf per dissected mosquito +1)",
     ylab = "log10(Mean of larvae per dissected mosquito +1)",
     col = palette[as.character(dat_sel$Mosquito_species)])
curve((.03*((x+1))^2.3520)/(1+((((x+1))^2.3520)/482.5680)), col = "black", add = TRUE)
# points(log10(dat_sel$mfE_mean_calculated+1), log10(dat_sel$Larvae_mean_calculated+1), col = "black", pch = "-")

# mfE --> Larvae, real data plot
plot(dat_sel$mfE_mean_calculated, dat_sel$Larvae_mean_arithmetic_perdissectedmosquito,
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Mean of ingested mf per dissected mosquito",
     ylab = "Mean of larvae per dissected mosquito",
     col = palette[as.character(dat_sel$Mosquito_species)])
curve((.03*x^2.3520)/(1+((x^2.3520)/482.5680)), col = "black", add = TRUE)
# points(dat_sel$mfE_mean_calculated, dat_sel$Larvae_mean_calculated, col = "black", pch = "-")

par(mfrow = c(1,1))



# Plots expected to be estimated ###############################################

# mfH --> InfectedMosquitoes(mfH)
par(mfrow = c(1,1), mar = c(4,5,2,2)) # margin (bottom,left,top,right)
plot(dat_sel$Human_mf_intensity_per20uL, (dat_sel$Mosquito_larvae_infected_count/dat_sel$Mosquito_totaldissected),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Human mf count per 20 uL",
     ylab = "Proportion of mosquitoes\nwith established infection",
     col = palette[as.character(dat_sel$Mosquito_species)])

# mfE --> InfectedMosquitoes(mfE)
par(mfrow = c(1,1), mar = c(4,5,2,2)) # margin (bottom,left,top,right)
plot(dat_sel$mfE_mean_calculated, (dat_sel$Mosquito_larvae_infected_count/dat_sel$Mosquito_totaldissected),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Mean of ingested mf per dissected mosquito",
     ylab = "Proportion of mosquitoes\nwith established infection",
     col = palette[as.character(dat_sel$Mosquito_species)])

# Larvae --> InfectedMosquitoes(Larvae)
par(mfrow = c(1,1), mar = c(4,5,2,2)) # margin (bottom,left,top,right)
plot(dat_sel$Larvae_mean_calculated, (dat_sel$Mosquito_larvae_infected_count/dat_sel$Mosquito_totaldissected),
     cex = dat_sel$Mosquito_totaldissected/50,
     xlab = "Mean of larvae per dissected mosquito",
     ylab = "Proportion of mosquitoes\nwith established infection",
     col = palette[as.character(dat_sel$Mosquito_species)])


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

par(mfrow = c(2,1), mar = c(4,5,2,2))
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
  dplyr::mutate(mean_mfE = mean(mfE_mean_calculated),
         var_mfE = var(mfE_mean_calculated),
         PearsonChiSq_mfE = (var_mfE-mean_mfE)/mean_mfE, # mfE shows overdispersion
         mean_Mosq = mean(Mosquito_larvae_infected_proportion_fromtotaldissected),
         var_Mosq = var(Mosquito_larvae_infected_proportion_fromtotaldissected),
         PearsonChiSq_Mosq = (var_Mosq-mean_Mosq)/mean_Mosq) %>% # Mosq shows underdispersion
  # view() %>% 
  glimpse()

