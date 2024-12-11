# Data viz!
library(tidyverse)
source("global/all_function.R") # Collected functions stored here!

# 1. Default parameters based on thesis ########################################
# (pending)
# An_g <- read.csv("outputs/odin_0defaut_An_gambiae.csv")
# An_a <- read.csv("outputs/odin_0defaut_An_arabiensis.csv", row.names = F)

# 2. Sensitivity Analyses ######################################################
# 2.1. I_H_per_H ###############################################################
An_g <- read.csv("outputs/odin_IHperH_An_gambiae.csv")
An_a <- read.csv("outputs/odin_IHperH_An_arabiensis.csv")

dat <- dplyr::left_join(An_g, An_a, by = "I_H_per_H", relationship = "many-to-many")
# x = An. gambiae (red)
# y = An. arabiensis (blue)

# Given all values of InfHuman
par(mfrow = c(1,2))
plot(dat$I_H_per_H, dat$Prev_loop.x, # An. gambiae
     xlab = "Proportion of LF in human population\n(with microfilaremia)",
     ylab = "Proportion of infective mosquitoes",
     main = "Proportion of infective mosquitoes",
     xlim = c(0,1), ylim = c(0,.4), type = "l", col = "red")
lines(dat$I_H_per_H, dat$Ang_low_prev, col = "red", lty = 2)
lines(dat$I_H_per_H, dat$Ang_up_prev, col = "red", lty = 2)

lines(dat$I_H_per_H, dat$Prev_loop.y, col = "blue") # An. arabiensis
lines(dat$I_H_per_H, dat$Ana_low_prev, col = "blue", lty = 2)
lines(dat$I_H_per_H, dat$Ana_up_prev, col = "blue", lty = 2)

plot(dat$I_H_per_H, dat$Pos_loop.x, # An. gambiae
     xlab = "Proportion of LF in human population\n(with microfilaremia)",
     ylab = "Proportion of positive mosquitoes",
     main = "Proportion of positive mosquitoes",
     xlim = c(0,1), ylim = c(0,.4), type = "l", col = "red")
lines(dat$I_H_per_H, dat$Ang_low_pos, col = "red", lty = 2)
lines(dat$I_H_per_H, dat$Ang_up_pos, col = "red", lty = 2)

lines(dat$I_H_per_H, dat$Pos_loop.y, col = "blue") # An. arabiensis
lines(dat$I_H_per_H, dat$Ana_low_pos, col = "blue", lty = 2)
lines(dat$I_H_per_H, dat$Ana_up_pos, col = "blue", lty = 2)
par(mfrow = c(1,1))


# ONLY InfHuman below WHO threshold (either it is .01 or .02)
dat_filtered <- dat %>% 
  dplyr::filter(I_H_per_H <= .025)

# Get data for dashed lines OR Points!
dat_mf_threshold <- dat %>% 
  dplyr::filter(I_H_per_H %in% c(0.01, 0.02)) %>% 
  dplyr::select(I_H_per_H,
                Prev_loop.x, Pos_loop.x,
                Prev_loop.y, Pos_loop.y) %>% 
  dplyr::distinct()


# Open df dedicated for CIs according to l1 & l2 logit function
dat_list <- list(
  read.csv("outputs/odin_IHperH_An_gambiae_low_CI.csv") %>% 
    dplyr::select(I_H_per_H, Prev_loop, Pos_loop) %>% 
    dplyr::rename(Ang_low_prev = Prev_loop,
                  Ang_low_pos = Pos_loop),
  read.csv("outputs/odin_IHperH_An_gambiae_up_CI.csv") %>% 
    dplyr::select(I_H_per_H, Prev_loop, Pos_loop) %>% 
    dplyr::rename(Ang_up_prev = Prev_loop,
                  Ang_up_pos = Pos_loop),
  read.csv("outputs/odin_IHperH_An_arabiensis_low_CI.csv") %>% 
    dplyr::select(I_H_per_H, Prev_loop, Pos_loop) %>% 
    dplyr::rename(Ana_low_prev = Prev_loop,
                  Ana_low_pos = Pos_loop),
  read.csv("outputs/odin_IHperH_An_arabiensis_up_CI.csv") %>% 
    dplyr::select(I_H_per_H, Prev_loop, Pos_loop) %>% 
    dplyr::rename(Ana_up_prev = Prev_loop,
                  Ana_up_pos = Pos_loop)
)
dat_CI <- purrr::reduce(dat_list, left_join, by = "I_H_per_H")

# Update dat
dat <- dplyr::left_join(dat, dat_CI, by = "I_H_per_H", relationship = "many-to-many")

# Infective mosquitoes
A_infective <- ggplot(dat, aes(x = I_H_per_H)) +
  geom_line(aes(y = Prev_loop.x, color = "An. gambiae s.s."), size = 1) +
  geom_line(aes(y = Prev_loop.y, color = "An. arabiensis"), size = 1) +
  geom_point(data = dat_mf_threshold, aes(x = I_H_per_H, y = Prev_loop.x, color = "An. gambiae s.s.")) +
  geom_ribbon(aes(ymin = Ang_low_prev, ymax = Ang_up_prev), fill = "red", colour = NA, alpha = 0.2) +
  geom_ribbon(aes(ymin = Ana_low_prev, ymax = Ana_up_prev), fill = "blue", colour = NA, alpha = 0.2) +
  geom_text(data = dat_mf_threshold, aes(x = I_H_per_H + 0.001,
                                         y = Prev_loop.x - 0.0003, 
                                         label = paste0("(", I_H_per_H*100, "%, ", round(Prev_loop.x*10000, 0), ")")), 
            color = "darkred", hjust = 0.5) +
  geom_point(data = dat_mf_threshold, aes(x = I_H_per_H, y = Prev_loop.y, color = "An. arabiensis")) +
  geom_text(data = dat_mf_threshold, aes(x = I_H_per_H - 0.001,
                                         y = Prev_loop.y + 0.0003, 
                                         label = paste0("(", I_H_per_H*100, "%, ", round(Prev_loop.y*10000, 0), ")")), 
            color = "darkblue", hjust = 0.5) +
  labs(title = "Infective Mosquitoes\n(per 10,000 Parous Mosquitoes)", 
       x = "Percentage of LF in human population\n(microfilaremia)",
       y = "Number of infective mosquitoes") +
  scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_y_continuous(labels = function(y) paste0(y * 10000)) +
  coord_cartesian(xlim = c(0, 0.023), ylim = c(0, 0.006)) +
  # xlim(0,.023) + ylim(0,.01) +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red", "An. arabiensis" = "blue"),
    labels = c(expression(italic("An. arabiensis")), expression(italic("An. gambiae") * " s.s."))
  ) +
  # theme_minimal() +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black"),
    
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.title = element_blank())

# Positive mosquitoes
B_positive <- ggplot(dat, aes(x = I_H_per_H)) +
  geom_line(aes(y = Pos_loop.x, color = "An. gambiae s.s."), size = 1) +
  geom_line(aes(y = Pos_loop.y, color = "An. arabiensis"), size = 1) +
  geom_point(data = dat_mf_threshold, aes(x = I_H_per_H, y = Pos_loop.x, color = "An. gambiae s.s.")) +
  geom_ribbon(aes(ymin = Ang_low_pos, ymax = Ang_up_pos), fill = "red", colour = NA, alpha = 0.2) +
  geom_ribbon(aes(ymin = Ana_low_pos, ymax = Ana_up_pos), fill = "blue", colour = NA, alpha = 0.2) +
  geom_text(data = dat_mf_threshold, aes(x = I_H_per_H + 0.001,
                                         y = Pos_loop.x - 0.0004, 
                                         label = paste0("(", I_H_per_H*100, "%, ", round(Pos_loop.x*10000, 0), ")")), 
            color = "darkred", hjust = 0.5) +
  geom_point(data = dat_mf_threshold, aes(x = I_H_per_H, y = Pos_loop.y, color = "An. arabiensis")) +
  geom_text(data = dat_mf_threshold, aes(x = I_H_per_H - 0.001,
                                         y = Pos_loop.y + 0.0005, 
                                         label = paste0("(", I_H_per_H*100, "%, ", round(Pos_loop.y*10000, 0), ")")), 
            color = "darkblue", hjust = 0.5) +
  labs(title = "Positive Mosquitoes\n(per 10,000 Parous Mosquitoes)", 
       x = "Percentage of LF in human population\n(microfilaremia)",
       y = "Number of positive mosquitoes") +
  scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_y_continuous(labels = function(y) paste0(y * 10000)) +
  coord_cartesian(xlim = c(0, 0.023), ylim = c(0, 0.008)) +
  # xlim(0,.023) + ylim(0,.02) +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red", "An. arabiensis" = "blue"),
    labels = c(expression(italic("An. arabiensis")), expression(italic("An. gambiae") * " s.s."))
  ) +
  # theme_minimal() +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black"),
    
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.title = element_blank())

png("pictures/odin_IHperH_modified_percent.png", width = 24, height = 12, unit = "cm", res = 1200)
cowplot::plot_grid(A_infective, B_positive, ncol = 2,
                   labels = c("A", "B")
                   # align = 'h'
)
dev.off()

# Version 2 when I slightly modify the axes

# Selected data for PCR pool calculations ######################################
selected_prevalences <- c(0.01, 0.015, 0.02, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)

# Filter the dat data frame for binom analysis
redefine_data <- dplyr::bind_rows(dat %>%
                                    dplyr::filter(I_H_per_H %in% selected_prevalences) %>%
                                    dplyr::mutate(species = "An. gambiae s.s.",
                                                  PCR_pool = 1,
                                                  p_positive = Pos_loop.x,
                                                  p_infective = Prev_loop.x) %>% 
                                    dplyr::select(I_H_per_H, species, PCR_pool, p_positive, p_infective),
                                  dat %>% 
                                    dplyr::filter(I_H_per_H %in% selected_prevalences) %>%
                                    dplyr::mutate(species = "An. arabiensis",
                                                  PCR_pool = 1,
                                                  p_positive = Pos_loop.y,
                                                  p_infective = Prev_loop.y) %>% 
                                    dplyr::select(I_H_per_H, species, PCR_pool, p_positive, p_infective)
) %>% 
  tidyr::pivot_longer(cols = starts_with("p_"),
                      names_to = "p_types",
                      values_to = "p_values") %>% 
  dplyr::mutate(P_confidence = 0.95) %>% 
  dplyr::bind_rows((.) %>% 
                     dplyr::mutate(P_confidence = 0.99)) %>% 
  dplyr::bind_rows((.) %>% 
                     dplyr::mutate(PCR_pool = 2),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 3),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 4),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 5),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 6),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 7),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 8),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 9),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 10),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 15),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 20),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 25),
                   (.) %>% 
                     dplyr::mutate(PCR_pool = 30))

binom_analyses <- redefine_data %>% 
  dplyr::mutate(n_1_binom = analyse_function(p = p_values, P = P_confidence, method = "binomial"),
                n_2_binom_pooled = analyse_function(p = p_values, P = P_confidence, method = "pooled_binomial", pool_size = PCR_pool),
                n_3_beta_binom_pooled = analyse_function(p = p_values, P = P_confidence, method = "pooled_beta_binomial", pool_size = PCR_pool, het = 0.01))

write.csv(binom_analyses, "outputs/odin_IHperH_report_table_n_pooled_mosquitoes_binom_analyses.csv", row.names = F)

# specified data chosen for P_confidence = 99%, PCR = 20 & 30 mosquitoes per pool, stratified by 1% OR 2% mf in human and mosquito species
filtered_binom_table <- binom_analyses %>% 
  dplyr::filter(I_H_per_H %in% c(0.01, 0.02),
                PCR_pool %in% c(20, 30),
                # p_types == "p_positive", # p_positive or p_infective
                P_confidence == 0.95) %>%  # 95% or 99%
  distinct(.keep_all = T) %>% 
  view()

write.csv(filtered_binom_table, "outputs/odin_IHperH_report_table_n_pooled_mosquitoes_binom_analyses_filtered.csv", row.names = F)


# Data viz!
# https://wilkelab.org/cowplot/articles/shared_legends.html

define_binom_methods <- c("n_1_binom", "n_2_binom_pooled", "n_3_beta_binom_pooled")
plot_title_positives <- list(n_1_binom = "Number of Dissected Mosquitoes that are Needed\ngiven p for Positive Mosquitoes (95% Confidence Level)",
                             n_2_binom_pooled = "Number of Pool that are Needed for PCR Analysis\ngiven p for Positive Mosquitoes (95% Confidence Level)",
                             n_3_beta_binom_pooled = "Number of Pool that are Needed for PCR Analysis, with Dispersion = 0.01\ngiven p for Positive Mosquitoes (95% Confidence Level)")

plot_title_infectives <- list(n_1_binom = "Number of Dissected Mosquitoes that are Needed\ngiven p for Infective Mosquitoes (95% Confidence Level)",
                             n_2_binom_pooled = "Number of Pool that are Needed for PCR Analysis\ngiven p for Infective Mosquitoes (95% Confidence Level)",
                             n_3_beta_binom_pooled = "Number of Pool that are Needed for PCR Analysis, with Dispersion = 0.01\ngiven p for Infective Mosquitoes (95% Confidence Level)")

# I only viz positive results (all LF stages in mosquitoes with 99& confidence)
for (y_col in define_binom_methods){
  
  filtered_binom_methods <- binom_analyses %>% 
    dplyr::filter(PCR_pool %in% c(5, 10, 15, 20, 25, 30),
                  p_types == "p_positive", # p_positive or p_infective
                  P_confidence == 0.95) # 95% or 99%
  
  title <- paste(plot_title_positives[[y_col]])
  file_path <- file.path("pictures", paste0("odin_IHperH_PCR_pool_positives_number_mosquitoes_", y_col, ".png"))
  
  png(file = file_path, width = 24, height = 12, unit = "cm", res = 1200)
  
  print_plot <- ggplot(filtered_binom_methods,
                       aes(x = I_H_per_H, y = .data[[y_col]], # 3 methods available
                           colour = PCR_pool, group = PCR_pool)) +
    geom_line(size = 1) +
    labs(title = title, 
         x = "Percentage of LF in human population\n(microfilaremia)",
         y = "Number") +
    scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
    coord_cartesian(xlim = c(0, 0.4)) +
    # xlim(0,0.4) + 
    theme_minimal(base_size = 15) + 
    theme(
      panel.grid.major = element_line(size = 0.2, color = "grey80"),
      panel.grid.minor = element_line(size = 0.2, color = "grey80"),
      axis.line = element_line(color = "black"),
      strip.text = element_text(face = "italic")
    ) +
    guides(colour = guide_legend(title = "PCR pool")) +
    facet_wrap(~ species, scales = "free_y")
  print(print_plot)
  
  dev.off()
}


# This time infective results (L3, or "established infection" stages in mosquitoes with 99& confidence)
for (y_col in define_binom_methods){
  
  filtered_binom_methods <- binom_analyses %>% 
    dplyr::filter(PCR_pool %in% c(5, 10, 15, 20, 25, 30),
                  p_types == "p_infective", # p_positive or p_infective
                  P_confidence == 0.95) # 95% or 99%
  
  title <- paste(plot_title_infectives[[y_col]])
  file_path <- file.path("pictures", paste0("odin_IHperH_PCR_pool_infectives_number_mosquitoes_", y_col, ".png"))
  
  png(file = file_path, width = 24, height = 12, unit = "cm", res = 1200)
  
  print_plot <- ggplot(filtered_binom_methods,
                       aes(x = I_H_per_H, y = .data[[y_col]], # 3 methods available
                           colour = PCR_pool, group = PCR_pool)) +
    geom_line(size = 1) +
    labs(title = title, 
         x = "Percentage of LF in human population\n(microfilaremia)",
         y = "Number") +
    scale_x_continuous(labels = function(x) paste0(x * 100, "%")) +
    coord_cartesian(xlim = c(0, 0.4)) +
    # xlim(0,0.4) + 
    theme_minimal(base_size = 15) + 
    theme(
      panel.grid.major = element_line(size = 0.2, color = "grey80"),
      panel.grid.minor = element_line(size = 0.2, color = "grey80"),
      axis.line = element_line(color = "black"),
      strip.text = element_text(face = "italic")
    ) +
    guides(colour = guide_legend(title = "PCR pool")) +
    facet_wrap(~ species, scales = "free_y")
  print(print_plot)
  
  dev.off()
}


# Filter the dat data frame to include only the selected prevalence values #####
filtered_data <- dplyr::bind_rows(dat %>%
                                    dplyr::filter(I_H_per_H %in% selected_prevalences) %>%
                                    dplyr::mutate(species = "An. gambiae s.s.",
                                                  PCR_pool = 1,
                                                  probability = Pos_loop.x,
                                                  positives = Pos_loop.x) %>% 
                                    dplyr::select(I_H_per_H, species, PCR_pool, probability, positives),
                                  dat %>% 
                                    dplyr::filter(I_H_per_H %in% selected_prevalences) %>%
                                    dplyr::mutate(species = "An. arabiensis",
                                                  PCR_pool = 1,
                                                  probability = Pos_loop.y,
                                                  positives = Pos_loop.y) %>% 
                                  dplyr::select(I_H_per_H, species, PCR_pool, probability, positives)
)

# Calculate filtered data
filtered_data_pool <- dplyr::bind_rows(filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 2,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 3,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 4,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 5,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 6,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 7,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 8,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 9,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 10,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 15,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 20,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 25,
                                                       probability = 1 - (1 - probability)^PCR_pool),
                                       filtered_data %>% 
                                         dplyr::mutate(PCR_pool = 30,
                                                       probability = 1 - (1 - probability)^PCR_pool),
)

# combine!
dat_pooled_calculations <- dplyr::bind_rows(filtered_data, filtered_data_pool)

# Data viz!
# https://wilkelab.org/cowplot/articles/shared_legends.html
InfHuman_vs_prob <- ggplot(dat_pooled_calculations %>% 
                             dplyr::filter(PCR_pool %in% c(5, 10, 15, 20)) #%>% 
                           # dplyr::mutate(probability = 1-probability) # Trial visualize the probability of mosquitoes are negative in a pool ((1-p)^n)
                           # dplyr::mutate(probability = (1-(1-probability)^(1/PCR_pool))^PCR_pool) # Trial visualize the probability when all mosquitoes are positive within a pool (p^n)
                           # dplyr::mutate(probability = 1-(1-(1-probability)^(1/PCR_pool))^PCR_pool) # Trial visualize the probability when at least have 1 negative mosquito within a pool (1-p^n)
                           ,
                           aes(x = I_H_per_H, y = probability,
                               colour = PCR_pool, group = PCR_pool)) +
  geom_line(size = 1) +
  labs(title = "Probability, given the proportion LF in human (with microfilaremia)", 
       x = "Proportion of LF in human population (microfilaremia)",
       y = "P(at least one positive mosquito)") +
  xlim(0,0.3) + ylim(0, max(dat_pooled_calculations$probability)) +
  # ylim(0, 0.25) +
  # theme_minimal() +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black"),
    strip.text = element_text(face = "italic")
  ) +
  guides(colour = guide_legend(title = "PCR pool")) +
  facet_wrap(~ species, scales = "free_y")
InfHuman_vs_prob


posmosq_vs_prob <- ggplot(dat_pooled_calculations %>% 
                             dplyr::filter(PCR_pool %in% c(5, 10, 15, 20)) #%>% 
                           # dplyr::mutate(probability = 1-probability) # Trial visualize the probability of mosquitoes are negative in a pool ((1-p)^n)
                           # dplyr::mutate(probability = (1-(1-probability)^(1/PCR_pool))^PCR_pool) # Trial visualize the probability when all mosquitoes are positive within a pool (p^n)
                           # dplyr::mutate(probability = 1-(1-(1-probability)^(1/PCR_pool))^PCR_pool) # Trial visualize the probability when at least have 1 negative mosquito within a pool (1-p^n)
                           ,
                           aes(x = positives, y = probability,
                               colour = PCR_pool, group = PCR_pool)) +
  geom_line(size = 1) +
  labs(title = "Probability, given the proportion of positive mosquitoes", 
       x = "Proportion of positive mosquitoes",
       y = "P(at least one positive mosquito)") +
  xlim(0,0.1614952) + ylim(0, max(dat_pooled_calculations$probability)) +
  # ylim(0, 0.25) +
  # theme_minimal() +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black"),
    strip.text = element_text(face = "italic")
  ) +
  guides(colour = guide_legend(title = "PCR pool")) +
  facet_wrap(~ species, scales = "free_y")
posmosq_vs_prob

png("pictures/odin_IHperH_PCR_pool.png", width = 24, height = 24, unit = "cm", res = 1200)
prow <- cowplot::plot_grid(
  InfHuman_vs_prob + theme(legend.position="none"),
  posmosq_vs_prob + theme(legend.position="bottom"),
  align = 'v',
  labels = c("A", "B"),
  hjust = -1,
  nrow = 2,
  rel_heights = c(1, 1.1)
)
prow
dev.off()

# How about calculating variance for pooled mosquitoes = 20?
variance_trial <- dat_pooled_calculations %>% 
  dplyr::mutate(std_dev_20 = sqrt(probability * (1 - probability)),
                lower_20 = probability - std_dev_20,
                upper_20 = probability + std_dev_20)

variance_trial$lower_20 <- pmax(variance_trial$lower_20, 0)
variance_trial$upper_20 <- pmin(variance_trial$upper_20, 1)


# Trial Plot
png("pictures/odin_IHperH_PCR_pool_20_with_stdev.png", width = 24, height = 12, unit = "cm", res = 1200)
pool_20_variance <- ggplot(variance_trial %>% 
                            dplyr::filter(PCR_pool ==20) #%>% 
                          # dplyr::mutate(probability = 1-probability) # Trial visualize the probability of mosquitoes are negative in a pool ((1-p)^n)
                          # dplyr::mutate(probability = (1-(1-probability)^(1/PCR_pool))^PCR_pool) # Trial visualize the probability when all mosquitoes are positive within a pool (p^n)
                          # dplyr::mutate(probability = 1-(1-(1-probability)^(1/PCR_pool))^PCR_pool) # Trial visualize the probability when at least have 1 negative mosquito within a pool (1-p^n)
                          ,
                          aes(x = positives, y = probability,
                              # colour = PCR_pool,
                              group = PCR_pool)) +
  geom_line(size = 1, colour = "darkblue") +
  geom_ribbon(aes(ymin = lower_20, ymax = upper_20), fill = "steelblue", colour = NA, alpha = 0.4) +
  labs(title = "Probability of at least 1 positive mosquito\nin pooled PCR analysis (n = 20, with STDEV)", 
       x = "Proportion of positive mosquitoes",
       y = "P(at least one positive mosquito)") +
  xlim(0,0.1614952) + ylim(min(variance_trial$lower_20)-0.05, max(variance_trial$upper_20)+0.05) +
  # ylim(0, 0.25) +
  # theme_minimal() +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black"),
    strip.text = element_text(face = "italic")
  ) +
  # guides(colour = guide_legend(title = "PCR pool")) +
  facet_wrap(~ species, scales = "free_y")
pool_20_variance
dev.off()


# Filter the dat data frame to include only the selected prevalence values
filtered_data <- dat %>%
  dplyr::filter(I_H_per_H %in% selected_prevalences) %>%
  dplyr::select(I_H_per_H, Prev_loop.x, Prev_loop.y, Pos_loop.x, Pos_loop.y) %>% 
  dplyr::mutate("Human mf prevalence" = paste0(I_H_per_H * 100, "%"),
                "An. gambiae s.s. with established infection per 10,000 (rounded 3)" = round(Prev_loop.x * 10000,3),
                "Positive An. gambiae s.s. per 10,000 (rounded 3)" = round(Pos_loop.x * 10000,3),
                "An. arabiensis with established infection per 10,000 (rounded 3)" = round(Prev_loop.y * 10000,3),
                "Positive An. arabiensis per 10,000 (rounded 3)" = round(Pos_loop.y * 10000,3),
                
                "Human mf prevalence" = paste0(I_H_per_H * 100, "%"),
                "An. gambiae s.s. with established infection per 10,000 (rounded 0)" = round(Prev_loop.x * 10000,0),
                "Positive An. gambiae s.s. per 10,000 (rounded 0)" = round(Pos_loop.x * 10000,0),
                "An. arabiensis with established infection per 10,000 (rounded 0)" = round(Prev_loop.y * 10000,0),
                "Positive An. arabiensis per 10,000 (rounded 0)" = round(Pos_loop.y * 10000,0),
                
                prob_PCR_An_g_5 = 1 - (1 - Pos_loop.x)^5,
                prob_PCR_An_a_5 = 1 - (1 - Pos_loop.y)^5, # assume pool size = 5 mosquitoes
                prob_PCR_An_g_7 = 1 - (1 - Pos_loop.x)^7,
                prob_PCR_An_a_7 = 1 - (1 - Pos_loop.y)^7, # assume pool size = 7 mosquitoes
                prob_PCR_An_g_10 = 1 - (1 - Pos_loop.x)^10,
                prob_PCR_An_a_10 = 1 - (1 - Pos_loop.y)^10, # assume pool size = 10 mosquitoes
                
                prob_PCR_An_g_15 = 1 - (1 - Pos_loop.x)^15,
                prob_PCR_An_a_15 = 1 - (1 - Pos_loop.y)^15, # assume pool size = 15 mosquitoes
                prob_PCR_An_g_20 = 1 - (1 - Pos_loop.x)^20,
                prob_PCR_An_a_20 = 1 - (1 - Pos_loop.y)^20, # assume pool size = 20 mosquitoes
                prob_PCR_An_g_25 = 1 - (1 - Pos_loop.x)^25,
                prob_PCR_An_a_25 = 1 - (1 - Pos_loop.y)^25, # assume pool size = 25 mosquitoes
                prob_PCR_An_g_30 = 1 - (1 - Pos_loop.x)^30,
                prob_PCR_An_a_30 = 1 - (1 - Pos_loop.y)^30 # assume pool size = 30 mosquitoes
                
                # Additional calculations
                # prob_PCR_An_g_50 = 1 - (1 - Pos_loop.x)^50,
                # prob_PCR_An_a_50 = 1 - (1 - Pos_loop.y)^50, # assume pool size = 50 mosquitoes
                # prob_PCR_An_g_75 = 1 - (1 - Pos_loop.x)^75,
                # prob_PCR_An_a_75 = 1 - (1 - Pos_loop.y)^75, # assume pool size = 75 mosquitoes
                # prob_PCR_An_g_100 = 1 - (1 - Pos_loop.x)^100,
                # prob_PCR_An_a_100 = 1 - (1 - Pos_loop.y)^100 # assume pool size = 100 mosquitoes
                # 
                ) %>%
  distinct(I_H_per_H, .keep_all = T)

view(t(filtered_data))

write.csv(t(filtered_data), "outputs/odin_IHperH_report_table_prob_and_pooled_calculations.csv", col.names = F)

t_table_data <- read.csv("outputs/odin_IHperH_report_table_prob_and_pooled_calculations.csv") #, header = F,
                         # col.names = c("Human mf prevalence", "1%", "1.5%", "2%", "5%", "10%", "15%", "20%", "25%", "30%", "35%", "40%"))


# 2.2.1. K #####################################################################
An_g <- read.csv("outputs/odin_K_An_gambiae.csv")
An_a <- read.csv("outputs/odin_K_An_arabiensis.csv")

dat <-  dplyr::bind_rows(An_g %>% 
                           dplyr::mutate(species = "An. gambiae s.s.")
                         ,
                         An_a %>% 
                           dplyr::mutate(species = "An. arabiensis")
) %>% 
  tidyr::pivot_longer(
    cols = c("Eggs", "Larvae", "Nullipars",
             "S_v_loop", "E_v_loop", "I_v_loop",
             "V_loop",
             "Prev_loop", "Pos_loop"),
    names_to = "mosquitoes_life_cycle",
    values_to = "n_mosquitoes"
  ) %>% 
  dplyr::mutate(
    mosquitoes_life_cycle = case_when(
      mosquitoes_life_cycle == "Nullipars" ~ "Nulliparous mosquitoes",
      mosquitoes_life_cycle == "S_v_loop" ~ "Susceptibles",
      mosquitoes_life_cycle == "E_v_loop" ~ "Exposed",
      mosquitoes_life_cycle == "I_v_loop" ~ "Established infection",
      
      mosquitoes_life_cycle == "V_loop" ~ "Total parous mosquitoes",
      mosquitoes_life_cycle == "Prev_loop" ~ "Proportion of mosquitoes\nwith established infection",
      mosquitoes_life_cycle == "Pos_loop" ~ "Proportion of\npositive mosquitoes",
      T ~ mosquitoes_life_cycle  # Keep others unchanged
    ),
    mosquitoes_life_cycle = factor(mosquitoes_life_cycle, levels = c("Eggs", "Larvae", "Nulliparous mosquitoes",
                                                                     "Susceptibles", "Exposed", "Established infection",
                                                                     "Total parous mosquitoes",
                                                                     "Proportion of mosquitoes\nwith established infection",
                                                                     "Proportion of\npositive mosquitoes")),)

png("pictures/odin_K.png", width = 28, height = 20, unit = "cm", res = 1200)
K_plot <- ggplot(dat,
                 aes(x = K, y = n_mosquitoes,
                     colour = species, group = species)) +
  geom_line(size = 1) +
  labs(title = "Number of Mosquitoes with Various Saturation Coefficient (K)", 
       x = "Saturation Coefficient (K)",
       y = "Value") +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red", "An. arabiensis" = "blue"),
    labels = c(expression(italic("An. arabiensis")), expression(italic("An. gambiae") * " s.s."))
  ) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black")
  ) +
  guides(colour = guide_legend(title = "Species")) +
  facet_wrap(~ mosquitoes_life_cycle, scales = "free_y")

K_plot # In case I wanna combine the pictures later with cowplot
dev.off()


# 2.2.2. K matrices ############################################################
An_g <- readRDS("outputs/odin_K_matrices_An_gambiae.rds")
An_a <- readRDS("outputs/odin_K_matrices_An_arabiensis.rds")

dat_V <- An_g$V_mtx
dat_Pos <- An_g$Pos_mtx
dat_Prev <- An_g$Prev_mtx

K_values <- seq(0, 268000, by = 100)
timesteps <- seq(0, 2000, by = 1)


head(dat_V)
tail(dat_V[,2679])
K_values[2679]

# LOG Plot for total (parous) mosquitoes
plot(timesteps,log10(dat_V[,2679]), type = "l", # V/H = 100
     xlim = c(0,50),
     xlab = "timesteps",
     ylab = "log10(total parous mosquitoes)",
     main = "The log proportion of total parous mosquitoes on various K")
lines(timesteps,log10(dat_V[,269]))  # V/H = 10
lines(timesteps,log10(dat_V[,28]))   # V/H = 1
lines(timesteps,log10(dat_V[,4]))    # V/H = 0.1

legend("topleft",
       legend = c(paste0("K = ", K_values[2679]), paste0("K = ", K_values[269]), paste0("K = ", K_values[28]), paste0("K = ", K_values[4])),
       cex = 0.8, bty = "n")

par(mfrow = c(1,2))
# LOG Plot for total positive (E_v + I_v) mosquitoes
plot(timesteps,dat_Prev[,2679], type = "l", # V/H = 100
     xlim = c(0,50),
     xlab = "timesteps",
     ylab = "Proportion of infective mosquitoes",
     main = "The proportion of infective mosquitoes on various K")
lines(timesteps,dat_Prev[,269])  # V/H = 10
lines(timesteps,dat_Prev[,28])   # V/H = 1
lines(timesteps,dat_Prev[,4])    # V/H = 0.1

legend("bottomright",
       legend = c(paste0("K = ", K_values[2679]), paste0("K = ", K_values[269]), paste0("K = ", K_values[28]), paste0("K = ", K_values[4])),
       cex = 0.8, bty = "n")

# Plot for total positive (E_v + I_v) mosquitoes
plot(timesteps,dat_Pos[,2679], type = "l", # V/H = 100
     xlim = c(0,50),
     xlab = "timesteps",
     ylab = "Proportion of positive mosquitoes)",
     main = "The proportion of positive mosquitoes on various K")
lines(timesteps,dat_Pos[,269])  # V/H = 10
lines(timesteps,dat_Pos[,28])   # V/H = 1
lines(timesteps,dat_Pos[,4])    # V/H = 0.1

legend("bottomright",
       legend = c(paste0("K = ", K_values[2679]), paste0("K = ", K_values[269]), paste0("K = ", K_values[28]), paste0("K = ", K_values[4])),
       cex = 0.8, bty = "n")

par(mfrow = c(1,1))


Predat_V <- An_g$I_v_mtx
# Plot for total infective (I_v) mosquitoes
plot(timesteps,Predat_V[,2679], type = "l", # V/H = 100
     xlim = c(0,50),
     xlab = "timesteps",
     ylab = "Proportion of infective mosquitoes")
lines(timesteps,Predat_V[,269])  # V/H = 10
lines(timesteps,Predat_V[,28])   # V/H = 1
lines(timesteps,Predat_V[,4])    # V/H = 0.1

legend("topleft",
       legend = c(paste0("K = ", K_values[2679]), paste0("K = ", K_values[269]), paste0("K = ", K_values[28]), paste0("K = ", K_values[4])),
       cex = 0.8, bty = "n")


# 2.3. epsilon #################################################################
An_g <- read.csv("outputs/odin_epsilon_An_gambiae.csv")
An_a <- read.csv("outputs/odin_epsilon_An_arabiensis.csv")

dat <-  dplyr::bind_rows(An_g %>% 
                           dplyr::mutate(species = "An. gambiae s.s.")
                         ,
                         An_a %>% 
                           dplyr::mutate(species = "An. arabiensis")
) %>% 
  tidyr::pivot_longer(
    cols = c("Eggs", "Larvae", "Nullipars",
             "S_v_loop", "E_v_loop", "I_v_loop",
             "V_loop",
             "Prev_loop", "Pos_loop"),
    names_to = "mosquitoes_life_cycle",
    values_to = "n_mosquitoes"
  ) %>% 
  dplyr::mutate(
    mosquitoes_life_cycle = case_when(
      mosquitoes_life_cycle == "Nullipars" ~ "Nulliparous mosquitoes",
      mosquitoes_life_cycle == "S_v_loop" ~ "Susceptibles",
      mosquitoes_life_cycle == "E_v_loop" ~ "Exposed",
      mosquitoes_life_cycle == "I_v_loop" ~ "Established infection",
      
      mosquitoes_life_cycle == "V_loop" ~ "Total parous mosquitoes",
      mosquitoes_life_cycle == "Prev_loop" ~ "Proportion of mosquitoes\nwith established infection",
      mosquitoes_life_cycle == "Pos_loop" ~ "Proportion of\npositive mosquitoes",
      T ~ mosquitoes_life_cycle  # Keep others unchanged
    ),
    mosquitoes_life_cycle = factor(mosquitoes_life_cycle, levels = c("Eggs", "Larvae", "Nulliparous mosquitoes",
                                                                     "Susceptibles", "Exposed", "Established infection",
                                                                     "Total parous mosquitoes",
                                                                     "Proportion of mosquitoes\nwith established infection",
                                                                     "Proportion of\npositive mosquitoes")),
    epsilon_day = round(3/epsilon, 3) # given epsilon = 1/(day/3)
  )

png("pictures/odin_epsilon.png", width = 28, height = 20, unit = "cm", res = 1200)
epsilon_plot <- ggplot(dat,
                       aes(x = epsilon_day, y = n_mosquitoes,
                           colour = species, group = species)) +
  geom_line(size = 1) +
  labs(title = bquote("Number of Mosquitoes with Various Extrinsic Incubation Rate (EIR, " * epsilon[V] * ")"),
       x = bquote("Extrinsic Incubation Rate (" * epsilon[V] * ") in day"),
       y = "Value") +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red", "An. arabiensis" = "blue"),
    labels = c(expression(italic("An. arabiensis")), expression(italic("An. gambiae") * " s.s."))
  ) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black")
  ) +
  guides(colour = guide_legend(title = "Species")) +
  facet_wrap(~ mosquitoes_life_cycle, scales = "free_y")

epsilon_plot # In case I wanna combine the pictures later with cowplot
dev.off()


# 2.4. gamma_L #################################################################
An_g <- read.csv("outputs/odin_gamma_L_An_gambiae.csv")
An_a <- read.csv("outputs/odin_gamma_L_An_arabiensis.csv")

dat <-  dplyr::bind_rows(An_g %>% 
                           dplyr::mutate(species = "An. gambiae s.s.")
                         ,
                         An_a %>% 
                           dplyr::mutate(species = "An. arabiensis")
) %>% 
  tidyr::pivot_longer(
    cols = c("Eggs", "Larvae", "Nullipars",
             "S_v_loop", "E_v_loop", "I_v_loop",
             "V_loop",
             "Prev_loop", "Pos_loop"),
    names_to = "mosquitoes_life_cycle",
    values_to = "n_mosquitoes"
  ) %>% 
  dplyr::mutate(
    mosquitoes_life_cycle = case_when(
      mosquitoes_life_cycle == "Nullipars" ~ "Nulliparous mosquitoes",
      mosquitoes_life_cycle == "S_v_loop" ~ "Susceptibles",
      mosquitoes_life_cycle == "E_v_loop" ~ "Exposed",
      mosquitoes_life_cycle == "I_v_loop" ~ "Established infection",
      
      mosquitoes_life_cycle == "V_loop" ~ "Total parous mosquitoes",
      mosquitoes_life_cycle == "Prev_loop" ~ "Proportion of mosquitoes\nwith established infection",
      mosquitoes_life_cycle == "Pos_loop" ~ "Proportion of\npositive mosquitoes",
      T ~ mosquitoes_life_cycle  # Keep others unchanged
    ),
    mosquitoes_life_cycle = factor(mosquitoes_life_cycle, levels = c("Eggs", "Larvae", "Nulliparous mosquitoes",
                                                                     "Susceptibles", "Exposed", "Established infection",
                                                                     "Total parous mosquitoes",
                                                                     "Proportion of mosquitoes\nwith established infection",
                                                                     "Proportion of\npositive mosquitoes"))
  )

png("pictures/odin_gamma_L.png", width = 28, height = 20, unit = "cm", res = 1200)
gamma_L_plot <- ggplot(dat,
                       aes(x = gamma_L, y = n_mosquitoes,
                           colour = species, group = species)) +
  geom_line(size = 1) +
  labs(title = bquote("Number of Mosquitoes with Various Effect of Saturation Coefficient (" * gamma[L] * ")"),
       x = bquote("Effect of saturation coefficient on late instars relative to early instars (" * gamma[L] * ")"),
       y = "Value") +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red", "An. arabiensis" = "blue"),
    labels = c(expression(italic("An. arabiensis")), expression(italic("An. gambiae") * " s.s."))
  ) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black")
  ) +
  guides(colour = guide_legend(title = "Species")) +
  facet_wrap(~ mosquitoes_life_cycle, scales = "free_y")

gamma_L_plot # In case I wanna combine the pictures later with cowplot
dev.off()


# 2.5. lambda ##################################################################
An_g <- read.csv("outputs/odin_lambda_An_gambiae.csv")
An_a <- read.csv("outputs/odin_lambda_An_arabiensis.csv")

dat <-  dplyr::bind_rows(An_g %>% 
                           dplyr::mutate(species = "An. gambiae s.s.")
                         ,
                         An_a %>% 
                           dplyr::mutate(species = "An. arabiensis")
) %>% 
  tidyr::pivot_longer(
    cols = c("Eggs", "Larvae", "Nullipars",
             "S_v_loop", "E_v_loop", "I_v_loop",
             "V_loop",
             "Prev_loop", "Pos_loop"),
    names_to = "mosquitoes_life_cycle",
    values_to = "n_mosquitoes"
  ) %>% 
  dplyr::mutate(
    mosquitoes_life_cycle = case_when(
      mosquitoes_life_cycle == "Nullipars" ~ "Nulliparous mosquitoes",
      mosquitoes_life_cycle == "S_v_loop" ~ "Susceptibles",
      mosquitoes_life_cycle == "E_v_loop" ~ "Exposed",
      mosquitoes_life_cycle == "I_v_loop" ~ "Established infection",
      
      mosquitoes_life_cycle == "V_loop" ~ "Total parous mosquitoes",
      mosquitoes_life_cycle == "Prev_loop" ~ "Proportion of mosquitoes\nwith established infection",
      mosquitoes_life_cycle == "Pos_loop" ~ "Proportion of\npositive mosquitoes",
      T ~ mosquitoes_life_cycle  # Keep others unchanged
    ),
    mosquitoes_life_cycle = factor(mosquitoes_life_cycle, levels = c("Eggs", "Larvae", "Nulliparous mosquitoes",
                                                                     "Susceptibles", "Exposed", "Established infection",
                                                                     "Total parous mosquitoes",
                                                                     "Proportion of mosquitoes\nwith established infection",
                                                                     "Proportion of\npositive mosquitoes"))
  )

png("pictures/odin_lambda.png", width = 28, height = 20, unit = "cm", res = 1200)
lambda_plot <- ggplot(dat,
                       aes(x = lambda, y = n_mosquitoes,
                           colour = species, group = species)) +
  geom_line(size = 1) +
  labs(title = bquote("Number of Mosquitoes with Various Bites (" * lambda * ")"),
       x = bquote("Number of bites per mosquitoe per cycle (" * lambda * ")"),
       y = "Value") +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red", "An. arabiensis" = "blue"),
    labels = c(expression(italic("An. arabiensis")), expression(italic("An. gambiae") * " s.s."))
  ) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black")
  ) +
  guides(colour = guide_legend(title = "Species")) +
  facet_wrap(~ mosquitoes_life_cycle, scales = "free_y")

lambda_plot # In case I wanna combine the pictures later with cowplot
dev.off()

# 2.6. mu0 #####################################################################
An_g <- read.csv("outputs/odin_mu0_An_gambiae.csv")
An_a <- read.csv("outputs/odin_mu0_An_arabiensis.csv")

dat <-  dplyr::bind_rows(An_g %>% 
                           dplyr::mutate(species = "An. gambiae s.s.")
                         ,
                         An_a %>% 
                           dplyr::mutate(species = "An. arabiensis")
) %>% 
  tidyr::pivot_longer(
    cols = c("Eggs", "Larvae", "Nullipars",
             "S_v_loop", "E_v_loop", "I_v_loop",
             "V_loop",
             "Prev_loop", "Pos_loop"),
    names_to = "mosquitoes_life_cycle",
    values_to = "n_mosquitoes"
  ) %>% 
  dplyr::mutate(
    mosquitoes_life_cycle = case_when(
      mosquitoes_life_cycle == "Nullipars" ~ "Nulliparous mosquitoes",
      mosquitoes_life_cycle == "S_v_loop" ~ "Susceptibles",
      mosquitoes_life_cycle == "E_v_loop" ~ "Exposed",
      mosquitoes_life_cycle == "I_v_loop" ~ "Established infection",
      
      mosquitoes_life_cycle == "V_loop" ~ "Total parous mosquitoes",
      mosquitoes_life_cycle == "Prev_loop" ~ "Proportion of mosquitoes\nwith established infection",
      mosquitoes_life_cycle == "Pos_loop" ~ "Proportion of\npositive mosquitoes",
      T ~ mosquitoes_life_cycle  # Keep others unchanged
    ),
    mosquitoes_life_cycle = factor(mosquitoes_life_cycle, levels = c("Eggs", "Larvae", "Nulliparous mosquitoes",
                                                                     "Susceptibles", "Exposed", "Established infection",
                                                                     "Total parous mosquitoes",
                                                                     "Proportion of mosquitoes\nwith established infection",
                                                                     "Proportion of\npositive mosquitoes"))
  )

png("pictures/odin_mu0.png", width = 28, height = 20, unit = "cm", res = 1200)
mu0_plot <- ggplot(dat,
                      aes(x = mu0, y = n_mosquitoes,
                          colour = species, group = species)) +
  geom_line(size = 1) +
  labs(title = bquote("Number of Mosquitoes with Various Mortality Rate of Early Instars (" * mu[0] * ")"),
       x = bquote("Per capita mortality rate of early instars (" * mu[0] * ")"),
       y = "Value") +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red", "An. arabiensis" = "blue"),
    labels = c(expression(italic("An. arabiensis")), expression(italic("An. gambiae") * " s.s."))
  ) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black")
  ) +
  guides(colour = guide_legend(title = "Species")) +
  facet_wrap(~ mosquitoes_life_cycle, scales = "free_y")

mu0_plot # In case I wanna combine the pictures later with cowplot
dev.off()

# Additional survivality/mortality viz #########################################
# Survival, Gompertz (Clements & Paterson, 1981) ###############################
# By using these parameters, age already in cycles,
# TRANSFIL time step (1 month) equals to 10 gonotrophic cycle of mosquitoes
# Gompertz mortality rate have already in cycle (Clements & Paterson, 1981)

source("model_dynamics/deterministic_odin.R") # Collected functions stored here!
data_viz <- function(species){
  species_params <- get_species(species)
  g1 = species_params$g1
  g2 = species_params$g2
  
  survpercycle <- rep(0,10)
  for (age in 0:10){
    survpercycle[age+1] = 1*exp(-g1/g2*(exp(age*g2)-1)) # survival per cycle with daily mortality update
  }
  return(survpercycle)
  
  # plot(seq(1,10,by=1),survpercycle, type = "l")
  
}

# Additional data for Culex quinquefasciatus for comparison
survpercycle <- numeric(11)  # that annoying dimension
for (age in 0:10) {
  survpercycle[age+1] = 1*exp(-0.572/0.513*(exp(age*0.513)-1))  # Adjust index for R's 1-based indexing
}
surv_Cx <- survpercycle
surv_Cx

surv_arabiensis <- data_viz("arabiensis")
surv_gambiae <- data_viz("gambiae")


# The viz using ggplot
gambiae_params <- get_species("gambiae")
arabiensis_params <- get_species("arabiensis")

data_combined <- data.frame(
  Age = rep(0:10, 3),
  Survival = c(surv_arabiensis, surv_gambiae, surv_Cx),
  Species = rep(c("An. arabiensis", "An. gambiae s.s.", "Cx. quinquefasciatus"), each = 11)
) %>% 
  glimpse()

# Plot using ggplot
png("pictures/odin_gompertz_survivability.png", width = 18, height = 10, unit = "cm", res = 1200)
ggplot(data_combined, aes(x = Age, y = Survival,
                          colour = Species,
                          # group = Species,
                          linetype = Species
                          )) +
  geom_line(size = 1) +
  # scale_color_manual(values = custom_colors) +
  scale_color_manual(
    values = c("An. gambiae s.s." = "red",
               "An. arabiensis" = "blue",
               "Cx. quinquefasciatus" = "darkgreen"),
    labels = c(expression(italic("An. arabiensis")),
               expression(italic("An. gambiae") * " s.s."),
               expression(italic("Cx. quinquefasciatus")))
  ) +
  geom_vline(xintercept = 4, linetype = "dotted", colour = "black") +
  geom_label(x = 4, y = 0.5, label = "EIR\n(12 days)", colour = "black") +
  annotate("text", x = 8, y = 0.5, label = "Infective\nState", angle = 0, vjust = -0.5, size = 5, color = "black") +
  scale_linetype_manual(values = c("An. arabiensis" = "solid",
                                   "An. gambiae s.s." = "solid",
                                   "Cx. quinquefasciatus" = "dashed"),
                        guide = "none") +
  labs(title = "Survival Probability of Mosquito Species",
       x = "Age (Gonotrophic Cycles)",
       y = "Probability of Survival") +
  # scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_x_continuous(breaks = 0:10, labels = as.character(0:10)) +  
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(size = 0.2, color = "grey80"),
    panel.grid.minor = element_line(size = 0.2, color = "grey80"),
    axis.line = element_line(color = "black"),
    strip.text = element_text(face = "italic"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  ) +
  guides(colour = guide_legend(title = "Species"))
dev.off()
