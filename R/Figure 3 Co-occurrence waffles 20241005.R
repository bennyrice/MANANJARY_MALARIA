library(tidyverse)
library(patchwork)
library(scales)
library(waffle)


####################################################################################################
## Read in data
####################################################################################################

#Malaria and nutrition indicators
df1 <- read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/processed_data/malaria_nutrition_data.csv")


####################################################################################################
# Trimming and tidying data # Malaria and nutrition indicators
####################################################################################################

df2 <- df1 %>% 
  #Trimming out rows with no data
  filter(!is.na(sample.date)) %>% 
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
                              site_code == "S6" ~ "MNJ.10"), .before = unique_ind_id) %>%
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
# Plot 1 # Malaria + MAM/SAM
####################################################################################################

#****fixes needed:
#*update legend names

df.mam <- df2 %>% filter(time_point != "T0") %>% 
  filter(!is.na(wasting.cat)) %>% filter(!is.na(rdt_result.coded)) %>%
  group_by(time_point, hh_id) %>% 
  summarize(n.mal.pos = sum(rdt_result.coded),
            n.mam     = sum(wasting.cat != "normal")) %>% ungroup() %>%
  group_by(time_point) %>%
  summarize(n = n(),
            n.mal_neg.mam_neg = length(which(n.mal.pos == 0 & n.mam == 0)),
            n.mal_neg.mam_pos = length(which(n.mal.pos == 0 & n.mam >  0)),
            n.mal_pos.mam_neg = length(which(n.mal.pos >  0 & n.mam == 0)),
            n.mal_pos.mam_pos = length(which(n.mal.pos >  0 & n.mam >  0))) %>%
  pivot_longer(!time_point:n, names_to = "category", values_to = "n.hh")

p.mam <- df.mam %>% ggplot(aes(fill = category, values = n.hh)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(vars(time_point), nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, # make this multiplier the same as n_rows
    expand = c(0,0)) +
  scale_fill_viridis_d(option = "inferno", end = 0.92, name = "Status",
                       labels = c("Malaria(-) Acute_Malnutrition(-)", "Malaria(-) Acute_Malnutrition(+)", "Malaria(+) Acute_Malnutrition(-)", "Malaria(+) Acute_Malnutrition(+)")
  ) +
  coord_equal() +
  labs(x = "Time Point", y = "Number of Households", 
       title = "Acute Malnutrition",
       subtitle = "Moderate or Severe Acute Malnutrition (MAM/SAM)") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(), legend.text = element_text(family = "Andale Mono")) +
  guides(fill = guide_legend(reverse = TRUE))
p.mam

####################################################################################################
# Plot 2 # Malaria + Food insecurity
####################################################################################################

df.csi <- df2 %>% filter(time_point != "T0") %>% 
  filter(!is.na(limit_portions)) %>% filter(!is.na(rdt_result.coded)) %>%
  group_by(time_point, hh_id) %>% 
  summarize(n.mal.pos = sum(rdt_result.coded),
            n.csi     = sum(limit_portions > 0)) %>% ungroup() %>%
  group_by(time_point) %>%
  summarize(n = n(),
            n.mal_neg.csi_neg = length(which(n.mal.pos == 0 & n.csi == 0)),
            n.mal_neg.csi_pos = length(which(n.mal.pos == 0 & n.csi >  0)),
            n.mal_pos.csi_neg = length(which(n.mal.pos >  0 & n.csi == 0)),
            n.mal_pos.csi_pos = length(which(n.mal.pos >  0 & n.csi >  0))) %>%
  pivot_longer(!time_point:n, names_to = "category", values_to = "n.hh")

p.csi <- df.csi %>% ggplot(aes(fill = category, values = n.hh)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(vars(time_point), nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, # make this multiplier the same as n_rows
    expand = c(0,0)) +
  scale_fill_viridis_d(option = "inferno", end = 0.92, name = "Status",
                       labels = c("Malaria(-) Food_Security(-)", "Malaria(-) Food_Security(+)", "Malaria(+) Food_Security(-)", "Malaria(+) Food_Security(+)")
  ) +
  coord_equal() +
  labs(x = "Time Point", y = "Number of Households", 
       title = "Food Insecurity",
       subtitle = "Reporting limiting portion size at meal time in the last week") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(), legend.text = element_text(family = "Andale Mono")) +
  guides(fill = guide_legend(reverse = TRUE))
p.csi


####################################################################################################
# Plot 3 # Malaria + Anemia
####################################################################################################

df.ane <- df2 %>% filter(time_point != "T0") %>% filter(time_point == "T09") %>%
  filter(!is.na(anemia.result)) %>% filter(anemia.result != "NA_Too_young") %>% filter(!is.na(rdt_result.coded)) %>%
  group_by(time_point, hh_id) %>% 
  summarize(n.mal.pos = sum(rdt_result.coded),
            n.ane     = sum(anemia.result != "0_Non_anemia")) %>% ungroup() %>%
  group_by(time_point) %>%
  summarize(n = n(),
            n.mal_neg.ane_neg = length(which(n.mal.pos == 0 & n.ane == 0)),
            n.mal_neg.ane_pos = length(which(n.mal.pos == 0 & n.ane >  0)),
            n.mal_pos.ane_neg = length(which(n.mal.pos >  0 & n.ane == 0)),
            n.mal_pos.ane_pos = length(which(n.mal.pos >  0 & n.ane >  0))) %>%
  pivot_longer(!time_point:n, names_to = "category", values_to = "n.hh")

p.ane <- df.ane %>% ggplot(aes(fill = category, values = n.hh)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(vars(time_point), nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, # make this multiplier the same as n_rows
    expand = c(0,0)) +
  scale_fill_viridis_d(option = "inferno", end = 0.92, name = "Status",
                       labels = c("Malaria(-) Anemia(-)", "Malaria(-) Anemia(+)", "Malaria(+) Anemia(-)", "Malaria(+) Anemia(+)")
  ) +
  coord_equal() +
  labs(x = "Time Point", y = "Number of Households", title = "Anemia") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(), legend.text = element_text(family = "Andale Mono")) +
  guides(fill = guide_legend(reverse = TRUE))
p.ane



(p.mam / p.csi) | p.ane


