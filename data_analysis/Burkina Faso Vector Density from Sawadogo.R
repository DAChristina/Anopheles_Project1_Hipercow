# Update: 01 January 2024

# QUICK NOTES:
# Finally I know what's difference with the data.
# The original *.xls file is in table format, not dataframe.
# I modify the format of the file suitable for dataframe format
# (not the content of the dataframe)


# USE ..._DC version for data analysis (modified for df format)
wd_R_vector_density = "/home/ron/Downloads/2023 Imperial MRes Journey/2023 Project1LF Mod TRANSFIL/BF Vector Density Data"
setwd(wd_R_vector_density)

library(tidyverse)
library(readxl)

file <- 'Database_LF_Sawadogo et al 2021_1DC_dfFormat.xls'
list_sheets <- excel_sheets(file)
all_data <- lapply(list_sheets, function(sheet){
  read_excel(file, sheet = sheet)
})

names(all_data) <- list_sheets
print(list_sheets)

# List of sheet names:
# [1] "General"            "Densitéstraps"      "Densitestraps_1_DC" "Densitéstraps (2)"  "Densitestraps_2_DC" "Base_Koudjo"       
# [7] "Base_Ouessa"        "Base_Bapla_clean"   "Base_LF"            "Infectivity rates"  "DC_NOTES"

# But we don't use "Densitéstraps" & "Densitéstraps (2)", use
# "Densitestraps_1_DC" & "Densitestraps_2_DC" instead

# View each sheet files as a different dataframes:
# lapply(all_data, function(df) view(df))

# 1. General sheet #############################################################
General_sum <- all_data$General %>% 
  group_by(Sites, `Collection methods`) %>% 
  summarise(total_mosquito = sum(`Number Mosquito`)) %>% 
  view()

# I don't know, the data on sheet "General" only consisting of 3 traps, not 4?
# The numbers also doesn't match to the published paper

# 2. Densitestraps_1_DC sheet ##################################################
view(all_data$Densitestraps_1_DC)

# Convert from wide to long data format:
Densitestraps_1_DC_long <- all_data$Densitestraps_1_DC %>% 
  gather(key = 'Species', value = 'Countt', -c(PIEGES,Village)) %>% 
  view()

Densitestraps_1_DC_long_sum <- Densitestraps_1_DC_long %>% 
  group_by(Village, PIEGES, Species) %>% 
  summarise(total_mosquito = sum(Countt)) %>%
  view()

# group_by(Village, PIEGES): Lah ini kok angkanya beda semua sama paper... ._.)"
# group_by(Village, Species): 4 species different from papers:
# An. funestus, An. pharoensis, Culex spp., Mansonia spp.

# 3. Densitestraps_2_DC sheet ##################################################
view(all_data$Densitestraps_2_DC)

# Convert from wide to long data format:
Densitestraps_2_DC_long <- all_data$Densitestraps_2_DC %>% 
  gather(key = 'Species', value = 'Countt', -c(PIEGES,Village)) %>% 
  view()

Densitestraps_2_DC_long_sum <- Densitestraps_2_DC_long %>% 
  group_by(Village, PIEGES, Species) %>% 
  summarise(total_mosquito = sum(Countt)) %>%
  view()

# group_by(Village, PIEGES): Ini juga sama angkanya beda sama paper... ._.)"
# group_by(Village, Species): also, 4 species different from papers:
# An. funestus, An. pharoensis, Culex spp., Mansonia spp.

# How about... differences between:
# Densitestraps_1_DC_long_sum vs. Densitestraps_2_DC_long_sum???
difference_1_vs_2 <- setdiff(Densitestraps_1_DC_long_sum, Densitestraps_2_DC_long_sum) %>% 
  view()

compare_1_vs_2 <- anti_join(Densitestraps_1_DC_long_sum, Densitestraps_2_DC_long_sum) %>% 
  view()

view(Densitestraps_1_DC_long_sum)
view(Densitestraps_2_DC_long_sum)

# It can be seen that the difference were in Bapla, EXCITTRAP, An. funestus:
# Densitestraps_1_DC_long_sum = 17 (count), while
# Densitestraps_2_DC_long_sum = 11 (count)
# There is the last line of EXCITRAP in sheets that makes a difference:
# 6 in Densitestraps_1 but 0 in Densitestraps_2

# 4. Base_Koudjo Sheet #########################################################
view(all_data$Base_Koudjo)

# Convert from wide to long data format:
Base_Koudjo_long <- all_data$Base_Koudjo %>% 
  gather(key = 'Species', value = 'Countt', -c(PIEGES)) %>% 
  view()

Base_Koudjo_long_sum <- Base_Koudjo_long %>% 
  group_by(PIEGES, Species) %>% 
  summarise(total_mosquito = sum(Countt)) %>%
  view()

# How about comparing all base sheets into filtered Densitestraps_2_DC_long_sum?
# Base_Koudjo_long_sum vs. Densitestraps_2_DC_long_sum_Koudjo
Densitestraps_2_DC_long_sum_Koudjo <- Densitestraps_2_DC_long_sum %>% 
  filter(Village == 'Koudjo') %>% 
  view()

compare_BaseKoudjo_vs_2 <- anti_join(Base_Koudjo_long_sum, Densitestraps_2_DC_long_sum_Koudjo) %>% 
  view()

view(Base_Koudjo_long_sum)
view(Densitestraps_2_DC_long_sum_Koudjo)

# This serves as a proof that data on Base_Koudjo has no differences in Densitestraps_2_DC

# 5. Base_Ouessa Sheet #########################################################
view(all_data$Base_Ouessa)

# Convert from wide to long data format:
Base_Ouessa_long <- all_data$Base_Ouessa %>% 
  gather(key = 'Species', value = 'Countt', -c(Trap)) %>% 
  view()

Base_Ouessa_long_sum <- Base_Ouessa_long %>% 
  group_by(Trap,Species) %>% 
  summarise(total_mosquito = sum(Countt)) %>%
  view()

# How about comparing all base sheets into filtered Densitestraps_2_DC_long_sum?
# Base_Ouessa_long_sum vs. Densitestraps_2_DC_long_sum_Ouessa
Densitestraps_2_DC_long_sum_Ouessa <- Densitestraps_2_DC_long_sum %>% 
  filter(Village == 'Ouessa') %>% 
  view()

compare_BaseOuessa_vs_2 <- anti_join(Base_Ouessa_long_sum, Densitestraps_2_DC_long_sum_Ouessa) %>% 
  view()

view(Base_Ouessa_long_sum)
view(Densitestraps_2_DC_long_sum_Ouessa)

# Base on R analysis, differences occur in 2 data point:
# 1. CSH, An. gambiae: Densitéstraps (2) = 588, Base_Ouessa = 693, in paper = 588
# 2. CSH, Autres: Densitéstraps (2) = NA, Base_Ouessa = 7, in paper = no Autres discussed

# 6. Base_Bapla_clean Sheet #########################################################
view(all_data$Base_Bapla_clean)

# Convert from wide to long data format:
Base_Bapla_clean_long <- all_data$Base_Bapla_clean %>% 
  gather(key = 'Species', value = 'Countt', -c(Traps, Location, `Indoor/Outdoor`,)) %>% 
  view()

Base_Bapla_clean_long_sum <- Base_Bapla_clean_long %>% 
  group_by(Traps,Species) %>% 
  summarise(total_mosquito = sum(Countt)) %>%
  view()

# I think both of data in Bapla is quite special because it has EFFAKARA trap value.
# Why they use EFFAKARA (LLINs) in Bapla but not in other areas?

# How about comparing all base sheets into filtered Densitestraps_2_DC_long_sum?
# Base_Bapla_clean_long_sum vs. Densitestraps_2_DC_long_sum_Bapla_clean
Densitestraps_2_DC_long_sum_Bapla_clean <- Densitestraps_2_DC_long_sum %>% 
  filter(Village == 'Bapla') %>% 
  rename(Traps = PIEGES) %>% 
  rename(total_mosquito_fromDensitetraps2 = total_mosquito) %>% 
  view()

compare_BaseBapla_vs_2 <- anti_join(Base_Bapla_clean_long_sum, Densitestraps_2_DC_long_sum_Bapla_clean) %>% 
  view()

# Base on R analysis, differences occur in MANY PLACES (16 data points)!
# The result is actually from Base_Bapla_clean.
# Coz' I'm a lazy person, I just wanna combine 2 df with differences in:
compare_BaseBapla_vs_2 <- compare_BaseBapla_vs_2 %>% 
  rename(total_mosquito_fromBaplaSheet = total_mosquito) %>% 
  view()

report_comparison <- 
  inner_join(compare_BaseBapla_vs_2, Densitestraps_2_DC_long_sum_Bapla_clean, by = c("Traps", "Species")) %>% 
  select(-Village) %>% 
  view()

# 7. Base_LF Sheet #########################################################
view(all_data$Base_LF)

# Convert from wide to long data format:
Base_LF_long <- all_data$Base_LF %>% 
  gather(key = 'Species', value = 'Countt', -c(Trap,Location,Period,`Indoor/Outdoor`)) %>% 
  view()

# group_by can be used to analyse other combinations, but let's set the group into
# Village, Species, Indoor/Outdoor (based on the paper):
Base_LF_long_sum <- Base_LF_long %>% 
  group_by(Location,Species,`Indoor/Outdoor`) %>% 
  summarise(total_mosquito = sum(Countt)) %>%
  view()

# Further analyses are needed,
# I don't know what is this sheet is trying to represent?
# Could it be used for mosquito density analysis per-month?





# PLOT Mosquitoes Only!!! ######################################################
# (line 932 ESPEN_DataViz.R)
# Mosquitoes_only is a mosquito data gathered from Sawadogo's paper separated from indoor and outdoor
library(tidyverse)
library(epitools)

Mosquitoes_only <- data.frame(year = c(2016, 2016, 2016, 2016),
                              village = c("Bapla", "Bapla", "Ouessa", "Ouessa"),
                              species = c("An. gambiae", "An. gambiae", "An. gambiae", "An. gambiae"),
                              site = c("Indoor", "Outdoor", "Indoor", "Outdoor"),
                              total = c(266, 328, 23, 36),
                              positive = c(0, 2, 1, 1))

Mosquitoes_only <- Mosquitoes_only %>% 
  mutate(Conf_Int = binom.exact(positive, total)) %>% 
  mutate(Calc_Proportion = positive/total,
         lo_CI = Conf_Int$lower,
         up_CI = Conf_Int$upper) # Show confint data

Mosquitoes_only$Threshold <- "0.6% threshold for positive mosquitoes"

# Plot for only Mosquitoes with CI:
ggplot(Mosquitoes_only, aes(x = site, y = Calc_Proportion,
                            ymin = lo_CI, ymax = up_CI)) +
  geom_point(shape=21, fill="lightblue", color="black", size=3) +
  # geom_hline(yintercept=.003, linetype="dashed", color = "red") + # 0.3% of infective An. gambiae from model (in proportion)
  geom_hline(aes(yintercept = 0.006, linetype = Threshold, color = Threshold), size = 1, show.legend = TRUE) +  # 0.6% of positive An. gambiae from model (in proportion)
  facet_wrap(~ village) +
  labs(x = "Site", y = "Proportion", title = "The proportion of positive mosquitoes in South-west Region in 2016") +
  geom_segment(aes(x = site, xend = site, y = lo_CI, yend = up_CI), color = "darkred") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        # legend.title = element_blank(),
        legend.position = "top",
        axis.text = element_text(size = 12),          # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold"),  # Adjust axis title size
        plot.title = element_text(size = 18, hjust = 0.5),   # Adjust plot title size
        strip.text = element_text(size = 12),        # Adjust facet label size
        legend.text = element_text(size = 12),       # Adjust legend text size
        legend.title = element_text(size = 0))      # Adjust legend title size
  scale_color_manual(name = "Threshold", values = "black") +
  scale_linetype_manual(name = "Threshold", values = "dashed") +
  # scale_x_continuous(breaks = 2016)
  scale_x_discrete()



