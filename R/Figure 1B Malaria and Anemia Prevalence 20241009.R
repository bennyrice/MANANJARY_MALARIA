library(tidyverse)
library(patchwork)
library(ggrepel)
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
# Plot 1 # Malaria prevalence at baseline
####################################################################################################

# Calculate malaria prevalence and confidence intervals at baseline T0 
df.mal <- df2 %>%
  #Dropping individuals with no malaria result
  filter(!is.na(rdt_result.coded)) %>%
  #Subsetting to baseline sample
  filter(time_point == "T0") %>%
  group_by(code.new, rdt_result.coded) %>%
  summarize(n = n(), .groups = 'drop') %>%
  group_by(code.new) %>%
  mutate(total = sum(n),
         prev = (n / total) * 100) %>%
  filter(rdt_result.coded == 1) %>%
  # Use binom.test to calculate the confidence interval for proportions
  mutate(conf_low  = (binom.test(n, total)$conf.int[1]) * 100,
         conf_high = (binom.test(n, total)$conf.int[2]) * 100)

p.mal <- df.mal %>%
  ggplot(aes(x = prev, y = reorder(code.new, desc(code.new)))) +
  geom_bar(stat = "identity", fill = "#AE123A") +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), 
                linewidth = 0.4,     # Thinner lines
                width = 0.1,        # Thinner horizontals on the Ts
                position = "identity") +
  xlab("Percent of sampled individuals positive for malaria (by RDT)\n(All ages)") + ylab(NULL) +
  xlim(0, 81) +
  theme_bw() + 
  theme(panel.grid = element_blank())
p.mal




####################################################################################################
# Plot 2 # Anemia prevalence
####################################################################################################



# Calculate anemia prevalence and confidence intervals
df.ane <- df2 %>%
  #Dropping individuals with no malaria result
  filter(!is.na(anemia.result)) %>%
  filter(anemia.result != "NA_Too_young") %>%
  # Recode anemia results
  mutate(anemia.result.y.n = case_when(
    anemia.result %in% c("1_Severe", "2_Moderate", "3_Mild") ~ 1,
    anemia.result == "0_Non_anemia" ~ 0)) %>%
  group_by(code.new, anemia.result.y.n) %>%
  summarize(n = n(), .groups = 'drop') %>%
  group_by(code.new) %>%
  mutate(total = sum(n),
         prev = (n / total) * 100) %>%
  filter(anemia.result.y.n == 1) %>%
  # Use binom.test to calculate the confidence interval for proportions
  mutate(conf_low  = (binom.test(n, total)$conf.int[1]) * 100,
         conf_high = (binom.test(n, total)$conf.int[2]) * 100)


p.ane <- df.ane %>%
  ggplot(aes(x = prev, y = reorder(code.new, desc(code.new)))) +
  geom_bar(stat = "identity", fill = "#C04F22") +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high), 
                linewidth = 0.4,     # Thinner lines
                width = 0.1,        # Thinner horizontals on the Ts
                position = "identity") +
  xlab("Percent of sampled individuals with anemia\n(All ages)") + ylab(NULL) +
  xlim(0, 81) +
  theme_bw() + 
  theme(panel.grid = element_blank())
p.ane

p.mal + p.ane



