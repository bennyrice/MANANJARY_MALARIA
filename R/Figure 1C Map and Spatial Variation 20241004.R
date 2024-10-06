library(tidyverse)
library(patchwork)
library(ggrepel)
library(geosphere)
library(sf)
library(terra)
library(scales)

####################################################################################################
## Read in data
####################################################################################################

df1 <- read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/processed_data/malaria_nutrition_data.csv")

####################################################################################################
## Trimming and tidying data
####################################################################################################

df2 <- df1 %>% 
  #Trimming out rows with no data
  filter(!is.na(sample.date)) %>% 
  #Dropping outliers
  filter(abs(hfaz) < 5) %>% filter(abs(wfaz) < 5) %>% filter(abs(wfhz) < 5) %>%
  #Factoring
  #Nutrition indicators
  mutate(stunting.cat    = factor(stunting.cat,    levels = rev(c("stunting",    "normal")))) %>%
  mutate(underweight.cat = factor(underweight.cat, levels = rev(c("underweight", "normal")))) %>%
  mutate(wasting.cat     = factor(wasting.cat,     levels = rev(c("SAM", "MAM",  "normal")))) %>%
  mutate(MUAC.cat        = factor(MUAC.cat,        levels = rev(c("SAM", "MAM",  "normal")))) %>%
  #Site codes
  mutate(site_code = factor(site_code, levels = rev(c("N1", "N2", "N3", "N4", "N5", "S1", "S2", "S3", "S5", "S6")))) %>%
  mutate(code.new = case_when(site_code == "N5" ~ "MNJ.01",
                              site_code == "N4" ~ "MNJ.02",
                              site_code == "N3" ~ "MNJ.03",
                              site_code == "N2" ~ "MNJ.04",
                              site_code == "N1" ~ "MNJ.05",
                              site_code == "S1" ~ "MNJ.06",
                              site_code == "S2" ~ "MNJ.07",
                              site_code == "S3" ~ "MNJ.08",
                              site_code == "S5" ~ "MNJ.09",
                              site_code == "S6" ~ "MNJ.10")) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.01", "MNJ.02", "MNJ.03", "MNJ.04", "MNJ.05", "MNJ.06", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.10"))) %>%
  #moving baseling time point to t0 and subsequent follow up time points to T01-T10
  mutate(time_point = case_when(
    time_point == "T01" ~ "T0",
    time_point == "T02" ~ "T01",
    time_point == "T03" ~ "T02",
    time_point == "T04" ~ "T03",
    time_point == "T05" ~ "T04",
    time_point == "T06" ~ "T05",
    time_point == "T07" ~ "T06",
    time_point == "T08" ~ "T07",
    time_point == "T09" ~ "T08",
    time_point == "T10" ~ "T09",
    time_point == "T11" ~ "T10"))


####################################################################################################
# Plot 1 # HFA z scores: Stunting
####################################################################################################

p1a <- df2 %>%
  ggplot(aes(x = hfaz, y = code.new, color = stunting.cat)) +
  geom_jitter(alpha = 0.5, width = 0) +
  scale_color_viridis_d(option = "inferno", name = "Stunting Outcome", begin = 0.07, end = 0.8) +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5), limits = c(-5, 5)) +
  xlab("Height for Age z-score") +
  ylab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid = element_blank())
p1a

####################################################################################################
# Plot 2 # WFA z scores: Underweight
####################################################################################################

p1b <- df2 %>%
  ggplot(aes(x = wfaz, y = code.new, color = underweight.cat)) +
  geom_jitter(alpha = 0.5, width = 0) +
  scale_color_viridis_d(option = "inferno", name = "Underweight Outcome", begin = 0.07, end = 0.8) +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5), limits = c(-5, 5)) +
  xlab("Weight for Age z-score") +
  ylab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid = element_blank())
p1b


####################################################################################################
# Plot 3 # WFH z scores: Wasting or MAM/SAM prevalence
####################################################################################################

p1c <- df2 %>%
  ggplot(aes(x = wfhz, y = code.new, color = wasting.cat)) +
  geom_jitter(alpha = 0.5, width = 0) +
  scale_color_viridis_d(option = "inferno", name = "Acute Malnutrition Outcome\n(Height for weight)", begin = 0.07, end = 0.8) +
  scale_x_continuous(breaks = c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5), limits = c(-5, 5)) +
  xlab("Weight for Height z-score") +
  ylab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid = element_blank())
p1c

####################################################################################################
# Plot 4 # MUAC z scores: MAM/SAM proxy
####################################################################################################

p1d <- df2 %>% filter(!is.na(PB)) %>% filter(!is.na(MUAC.cat)) %>%
  ggplot(aes(x = PB, y = code.new, color = MUAC.cat)) +
  geom_jitter(alpha = 0.5, width = 0.1) +
  scale_color_viridis_d(option = "inferno", name = "Acute Malnutrition Outcome\n(MUAC)", begin = 0.07, end = 0.8) +
  xlab("MUAC (cm)") +
  ylab(NULL) +
  theme_bw() +
  theme(legend.position = "bottom", panel.grid = element_blank())
p1d


p1a + p1b + p1c + p1d + plot_layout(nrow = 1)


