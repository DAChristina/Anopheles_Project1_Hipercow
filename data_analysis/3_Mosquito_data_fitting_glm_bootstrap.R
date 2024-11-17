# bootstrapped confidence interval
# SOURCE: https://epurdom.github.io/Stat131A/book/curve-fitting.html

# 1. Data used mfE vs. Mosquito proportion with Larvae #########################
# Data needed:
# dat_sel$Human_mf_intensity_per20uL
# dat_sel$Mosquito_larvae_infected_proportion_fromtotaldissected
# dat_sel$mf_mean_arithmetic_pertotaldissectedmosquito
# dat_sel$mfE_mean_calculated

# 1. Data preparation ##########################################################
library(tidyverse)
library(readxl)

dat_sel <- read.csv("inputs/Mosquito_EstablishedInfection_Data_clean.csv")

# 2. SUCCESSSS glm #############################################################

# 2.1. Use mfE vs. Proportion, ALL SPECIES #####################################
dat_anal <- dat_sel %>% 
  select(Reference, Mosquito_species, Mosquito_larvae_infected_proportion_fromtotaldissected, Mosquito_totaldissected, Mosquito_larvae_infected_count, mfE_mean_calculated) %>% 
  filter(!is.na(Mosquito_larvae_infected_proportion_fromtotaldissected) & Mosquito_larvae_infected_proportion_fromtotaldissected != "not_analysed",
         !is.na(mfE_mean_calculated) & mfE_mean_calculated != "not_analysed") %>%
  mutate(Unique_ID = row_number()) %>% 
  # view() %>% 
  glimpse()

# To simplify the code
y <- dat_anal$Mosquito_larvae_infected_proportion_fromtotaldissected
x <- dat_anal$mfE_mean_calculated
AllMosq <- dat_anal$Mosquito_totaldissected
AllPos <- dat_anal$Mosquito_larvae_infected_count

# THE BOOTSTRAP! ###############################################################
bootstrapGLM <- function(y, x, repetitions, confidence.level = 0.95, seed = 0) {
  set.seed(seed)  # For reproducibility
  fit <- glm(cbind(AllPos, AllMosq - AllPos) ~ x, family = binomial(link = "logit"))
  stat.obs <- coef(fit)
  sum.all <- summary(fit)
  std.Err <- sum.all$coefficients[, "Std. Error"] # that pain-in-the ass Std.Err coz examples only calculate CI
  
  # Predict fitted values for the original data
  fitted_values_original <- predict(fit, type = "response")  # Logistic predictions
  
  # Bootstrap function
  bootFun <- function() {
    sampled <- sample(1:length(y), size = length(y), replace = TRUE)
    fit_sampled <- glm(cbind(AllPos[sampled], AllMosq[sampled] - AllPos[sampled]) ~ x[sampled], family = binomial(link = "logit"))
    
    list(coef = coef(fit_sampled), fitted = predict(fit_sampled, type = "response")) # Return from the resampled model
  }
  
  stat.boot <- replicate(repetitions, bootFun(), simplify = FALSE)
  
  boot.coefs <- sapply(stat.boot, `[[`, "coef")
  boot.fitted <- do.call(cbind, lapply(stat.boot, `[[`, "fitted"))
  
  level <- 1 - confidence.level
  confidence.interval <- apply(boot.coefs, 1, quantile, probs = c(level/2, 1 - level/2))
  
  # Confidence intervals for fitted values
  fitted.lower <- apply(boot.fitted, 1, quantile, probs = level/2)
  fitted.upper <- apply(boot.fitted, 1, quantile, probs = 1 - level/2)
  
  # Return bootstrapped results: coefficients, fitted values, and their confidence intervals
  return(list(
    confidence.interval = cbind(lower = confidence.interval[1,], 
                                estimate = stat.obs, 
                                upper = confidence.interval[2,],
                                stdError = std.Err),
    bootStats = boot.coefs,  # Bootstrapped coefficients
    fittedValues = list(original = fitted_values_original, lower = fitted.lower, upper = fitted.upper)
  ))
}

logit <- function(x,l1,l2) { # Translated from the log-odds function from b0 = l1 & b1 = l2
  1/(1+exp(-(l1+l2*x)))
}

reverse_logit <- function(y, l1, l2) { # To find the value of x (ingested mf), given y = 50%
  (log(y/(1-y))-l1)/l2
}

# Call bootstrap with ORI data
result <- bootstrapGLM(y = cbind(AllPos, AllMosq - AllPos), 
                       x = dat_anal$mfE_mean_calculated, 
                       repetitions = 1000, 
                       confidence.level = 0.95,
                       seed = 0)
# result

# PLOT #########################################################################
# Confidence intervals
private_conf <- result$confidence.interval

# Bootstapped estim values (averaged)
private_estim <- rowMeans(result$bootStats)

# Save df result
combined_all <- cbind(private_conf, data.frame(estimate_glm_bootstrapped = as.numeric(private_estim))) %>% 
  mutate(mosquito = "all")

# Define parameters for logit function
lower <- private_conf[,1]
estim <- private_estim # the equation! (instead of private_conf[,2])
upper <- private_conf[,3]

# Plot the data
png("pictures/propvsave_1_all.png", width = 17, height = 12, unit = "cm", res = 1200)
par(mar = c(4, 5, 1, 1)) # bottom, left, top, right
plot(x,y, las = 1, pch = 1,
     xlab = "Average of ingested mf in mosquitoes", ylab = "Proportion of dissected mosquitoes\nwith established larvae",
     xlim = c(0,10), ylim = c(0,1), col = "grey50", cex = AllMosq/80)

curve(logit(x, estim[1], estim[2]), col = "grey10", add = TRUE, lwd = 2) # the equation based on bootstrap
curve(logit(x, upper[1], upper[2]), col = "grey35", add = TRUE, lty = 2) # Upper bound
curve(logit(x, lower[1], lower[2]), col = "grey35", add = TRUE, lty = 2) # Lower bound
dev.off()


# 2.2. Use mfE vs. Proportion, An. gambiae #####################################
# par(mfrow = c(1,2)) # for plotting together An. g & An. a in one output
dat_anal <- dat_sel %>% 
  select(Mosquito_species, Mosquito_larvae_infected_proportion_fromtotaldissected, Mosquito_totaldissected, Mosquito_larvae_infected_count, mfE_mean_calculated) %>% 
  filter(!is.na(Mosquito_larvae_infected_proportion_fromtotaldissected) & Mosquito_larvae_infected_proportion_fromtotaldissected != "not_analysed",
         !is.na(mfE_mean_calculated) & mfE_mean_calculated != "not_analysed",
         Mosquito_species == "An. gambiae") %>%
  mutate(Unique_ID = row_number()) %>% 
  # view() %>% 
  glimpse()

# To simplify my life a bit
y <- dat_anal$Mosquito_larvae_infected_proportion_fromtotaldissected
x <- dat_anal$mfE_mean_calculated
AllMosq <- dat_anal$Mosquito_totaldissected
AllPos <- dat_anal$Mosquito_larvae_infected_count

# THE BOOTSTRAP! ###############################################################
# bootstrapGLM have defined previously

# Call bootstrap with ORI data
result <- bootstrapGLM(y = cbind(AllPos, AllMosq - AllPos), 
                       x = dat_anal$mfE_mean_calculated, 
                       repetitions = 1000, 
                       confidence.level = 0.95,
                       seed = 0)
# result

# PLOT #########################################################################
# Confidence intervals
private_conf <- result$confidence.interval

# Bootstapped estim values (averaged)
private_estim <- rowMeans(result$bootStats)

# Save df result
combined_Ang <- cbind(private_conf, data.frame(estimate_glm_bootstrapped = as.numeric(private_estim))) %>% 
  mutate(mosquito = "An. gambiae")

# Define parameters for logit function
lower <- private_conf[,1]
estim <- private_estim # the equation! (instead of private_conf[,2])
upper <- private_conf[,3]

# Plot the data
png("pictures/propvsave_2_Angambiae.png", width = 17, height = 12, unit = "cm", res = 1200)
par(mar = c(4, 5, 1, 1)) # bottom, left, top, right
plot(x,y, las = 1, pch = 1,
     xlab = "Average of ingested mf in mosquitoes", ylab = "Proportion of dissected mosquitoes\nwith established larvae",
     xlim = c(0,10), ylim = c(0,1), col = "red", cex = AllMosq/80)

curve(logit(x, estim[1], estim[2]), col = "darkred", add = TRUE, lwd = 2) # the equation based on bootstrap
curve(logit(x, upper[1], upper[2]), col = "violetred1", add = TRUE, lty = 2) # Upper bound
curve(logit(x, lower[1], lower[2]), col = "violetred1", add = TRUE, lty = 2) # Lower bound
dev.off()


# 2.3. Use mfE vs. Proportion, An. arabiensis #####################################
dat_anal <- dat_sel %>% 
  select(Mosquito_species, Mosquito_larvae_infected_proportion_fromtotaldissected, Mosquito_totaldissected, Mosquito_larvae_infected_count, mfE_mean_calculated) %>% 
  filter(!is.na(Mosquito_larvae_infected_proportion_fromtotaldissected) & Mosquito_larvae_infected_proportion_fromtotaldissected != "not_analysed",
         !is.na(mfE_mean_calculated) & mfE_mean_calculated != "not_analysed",
         Mosquito_species == "An. arabiensis") %>%
  mutate(Unique_ID = row_number()) %>% 
  # view() %>% 
  glimpse()

# To simplify my life a bit
y <- dat_anal$Mosquito_larvae_infected_proportion_fromtotaldissected
x <- dat_anal$mfE_mean_calculated
AllMosq <- dat_anal$Mosquito_totaldissected
AllPos <- dat_anal$Mosquito_larvae_infected_count

# THE BOOTSTRAP! ###############################################################
# THE BOOTSTRAP! ###############################################################
# bootstrapGLM have defined previously

# Call bootstrap with ORI data
result <- bootstrapGLM(y = cbind(AllPos, AllMosq - AllPos), 
                       x = dat_anal$mfE_mean_calculated, 
                       repetitions = 1000, 
                       confidence.level = 0.95,
                       seed = 0)
# result

# PLOT #########################################################################
# Confidence intervals
private_conf <- result$confidence.interval

# Bootstapped estim values (averaged)
private_estim <- rowMeans(result$bootStats)

# Save df result
combined_Ana <- cbind(private_conf, data.frame(estimate_glm_bootstrapped = as.numeric(private_estim))) %>% 
  mutate(mosquito = "An. arabiensis")

# Define parameters for logit function
lower <- private_conf[,1]
estim <- private_estim # the equation! (instead of private_conf[,2])
upper <- private_conf[,3]

# Plot the data
png("pictures/propvsave_3_Anarabiensis.png", width = 17, height = 12, unit = "cm", res = 1200)
par(mar = c(4, 5, 1, 1)) # bottom, left, top, right
plot(x,y, las = 1, pch = 1,
     xlab = "Average of ingested mf in mosquitoes", ylab = "Proportion of dissected mosquitoes\nwith established larvae",
     xlim = c(0,10), ylim = c(0,1), col = "blue", cex = AllMosq/80)

curve(logit(x, estim[1], estim[2]), col = "darkblue", add = TRUE, lwd = 2) # the equation based on bootstrap
curve(logit(x, upper[1], upper[2]), col = "steelblue", add = TRUE, lty = 2) # Upper bound
curve(logit(x, lower[1], lower[2]), col = "steelblue", add = TRUE, lty = 2) # Lower bound
dev.off()


# 2.4. Use mfE vs. Proportion, An. melas #######################################
dat_anal <- dat_sel %>% 
  select(Mosquito_species, Mosquito_larvae_infected_proportion_fromtotaldissected, Mosquito_totaldissected, Mosquito_larvae_infected_count, mfE_mean_calculated) %>% 
  filter(!is.na(Mosquito_larvae_infected_proportion_fromtotaldissected) & Mosquito_larvae_infected_proportion_fromtotaldissected != "not_analysed",
         !is.na(mfE_mean_calculated) & mfE_mean_calculated != "not_analysed",
         Mosquito_species == "An. melas") %>%
  mutate(Unique_ID = row_number()) %>% 
  # view() %>% 
  glimpse()

# To simplify my life a bit
y <- dat_anal$Mosquito_larvae_infected_proportion_fromtotaldissected
x <- dat_anal$mfE_mean_calculated
AllMosq <- dat_anal$Mosquito_totaldissected
AllPos <- dat_anal$Mosquito_larvae_infected_count

# THE BOOTSTRAP! ###############################################################
# THE BOOTSTRAP! ###############################################################
# bootstrapGLM have defined previously

# Call bootstrap with ORI data
result <- bootstrapGLM(y = cbind(AllPos, AllMosq - AllPos), 
                       x = dat_anal$mfE_mean_calculated, 
                       repetitions = 1000, 
                       confidence.level = 0.95,
                       seed = 0)
# result

# PLOT #########################################################################
# Confidence intervals
private_conf <- result$confidence.interval

# Bootstapped estim values (averaged)
private_estim <- rowMeans(result$bootStats)

# Save df result
combined_Anm <- cbind(private_conf, data.frame(estimate_glm_bootstrapped = as.numeric(private_estim))) %>% 
  mutate(mosquito = "An. melas")

combined_final <- rbind(combined_all, combined_Ang, combined_Ana, combined_Anm)

write.csv(combined_final, "outputs/mosquitoes_logit_parameters.csv", row.names = T)

# Define parameters for logit function
lower <- private_conf[,1]
estim <- private_estim # the equation! (instead of private_conf[,2])
upper <- private_conf[,3]

# Plot the data
png("pictures/propvsave_4_Anmelas.png", width = 17, height = 12, unit = "cm", res = 1200)
par(mar = c(4, 5, 1, 1)) # bottom, left, top, right
plot(x,y, las = 1, pch = 1,
     xlab = "Average of ingested mf in mosquitoes", ylab = "Proportion of dissected mosquitoes\nwith established larvae",
     xlim = c(0,10), ylim = c(0,1), col = "forestgreen", cex = AllMosq/80)

curve(logit(x, estim[1], estim[2]), col = "darkgreen", add = TRUE, lwd = 2) # the equation based on bootstrap
curve(logit(x, upper[1], upper[2]), col = "darkseagreen4", add = TRUE, lty = 2) # Upper bound
curve(logit(x, lower[1], lower[2]), col = "darkseagreen4", add = TRUE, lty = 2) # Lower bound
dev.off()


# Additional cowpar(mar = c(4, 5, 1, 1)) # bottom, left, top, right for three Anopheles species (to be continued)