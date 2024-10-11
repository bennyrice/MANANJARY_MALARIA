library(tidyverse)
library(patchwork)
library(scales)
library(waffle)


####################################################################################################
## Read in data
####################################################################################################

#Malaria and nutrition indicators
df1 <- read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/processed_data/malaria_nutrition_data.csv")
#Sampling dates
dfs.i <- read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/raw_data/malaria_rdt_summary/sample_dates.csv")


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
# Trimming and tidying data # Sampling timing
####################################################################################################

dfs1 <- dfs.i %>%   mutate(code.new = case_when(site_code == "N5" ~ "MNJ.01",
                                                site_code == "N4" ~ "MNJ.02",
                                                site_code == "N3" ~ "MNJ.03",
                                                site_code == "N2" ~ "MNJ.04",
                                                site_code == "N1" ~ "MNJ.05",
                                                site_code == "S1" ~ "MNJ.06",
                                                site_code == "S2" ~ "MNJ.07",
                                                site_code == "S3" ~ "MNJ.08",
                                                site_code == "S5" ~ "MNJ.09",
                                                site_code == "S6" ~ "MNJ.10"), .before = site_name) %>%
  mutate(code.new = factor(code.new, levels = c("MNJ.01", "MNJ.02", "MNJ.03", "MNJ.04", "MNJ.05", "MNJ.06", "MNJ.07", "MNJ.08", "MNJ.09", "MNJ.10"))) %>%
  #Pivoting longer
  pivot_longer(!site_code:site_name, names_to = "time_point.day", values_to = "date.char") %>%
  mutate(date = dmy(date.char)) %>%
  mutate(time_point = substr(time_point.day, 1, 3)) %>%
  arrange(time_point, code.new) %>%
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


dfs.by_time_point <- dfs1 %>% filter(!is.na(date)) %>% group_by(time_point) %>% summarize(min.date = min(date), max.date = max(date))


####################################################################################################
# Plot 1 # Malaria + MAM/SAM
####################################################################################################


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


v.inferno.20 <- viridis_pal(option = "inferno")(20)

p.mam <- df.mam %>% ggplot(aes(fill = category, values = n.hh)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(vars(time_point), nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, # make this multiplier the same as n_rows
    expand = c(0,0)) +
  scale_fill_manual(name = "Household Status",
                    labels = c("Malaria(-) Acute Malnutrition(-)", "Malaria(-) Acute Malnutrition(+)", "Malaria(+) Acute Malnutrition(-)", "Malaria(+) Acute Malnutrition(+)"),
                    values = c(v.inferno.20[2], v.inferno.20[10], v.inferno.20[14], v.inferno.20[18])) +
  coord_equal() +
  xlab("Time Point") +
  ylab("Count of Households") +
   theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(), legend.text = element_text(family = "Andale Mono"),
        legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE))
p.mam


#For visualizing time series
p.dfs <- dfs.by_time_point %>% filter(time_point != "T0") %>%
  mutate(months = paste0(substr(min.date, 1, 7), " to ", substr(max.date, 1, 7))) %>%
  ggplot(aes(x = time_point, y = 1)) +
  geom_text(aes(label = months), angle = 90, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(label = time_point, y = 1.5), angle = 0, hjust = 0.5, vjust = 0.5) +
  ylim(0, 2) +
  theme_void() +
  theme()
p.dfs

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
  scale_fill_manual(name = "Household Status",
                    labels = c("Malaria(-) Food Insecurity(-)", "Malaria(-) Food Insecurity(+)", "Malaria(+) Food Insecurity(-)", "Malaria(+) Food Insecurity(+)"),
                    values = c(v.inferno.20[2], v.inferno.20[10], v.inferno.20[14], v.inferno.20[18])) +
  coord_equal() +
  xlab("Time Point") +
  ylab("Count of Households") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(), legend.text = element_text(family = "Andale Mono"),
        legend.position = "top") +
  guides(fill = guide_legend(reverse = TRUE))
p.csi


####################################################################################################
# Supplement plot if needed # Malaria + Anemia
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




