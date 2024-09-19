library(tidyverse)
library(patchwork)
library(zscorer)

####################################################################################################################################
#Playing with height and weight data

#Reading in data
dfi <- readr::read_csv("/Users/blrice/Library/CloudStorage/Dropbox/R DROPBOX/2022_MNJ_TAZO/data_rdt/rdt_anthro_data_20230707.csv") 
#Removing unnecessary column
dfi <- dfi %>% dplyr::select(-enrollment_status)

#Recoding sex: 1 (male) or 2 (female)
#Recoding age in days
df1 <- dfi %>% mutate(sex = case_when(sex == "M" ~ 1, sex == "F" ~ 2)) %>%
  mutate(age.days = age.yrs.at.sample*365.25)

#Filter out adults and NAs
df2 <- df1 %>% filter(!is.na(KG)) %>% filter(!is.na(CM)) %>% filter(age.days < 6940)

#Calculations: hfa, wfa, wfh

#Weight-for-height (wfh) z-scores for children with heights between 65 and 120 cm
#(Wasting)
#SAM: wfhz score <-3
#MAM: wfhz score ≥ -3 and < -2 z-score
df.wfh.i <- addWGSR(data = df2, sex = "sex", firstPart = "KG", secondPart = "CM", index = "wfh") 
df.wfh <- df.wfh.i %>% filter(!is.na(wfhz)) %>% filter(wfhz <= 5) %>% filter(wfhz >= -5) %>% filter(!is.na(rdt.result))
df.wfh <- df.wfh %>% mutate(wasting.cat = case_when(
  wfhz >= -2 ~ "normal",
  wfhz <  -2 & wfhz >= -3 ~ "MAM",
  wfhz <  -3 ~ "SAM")) %>% 
  mutate(wasting.cat=factor(wasting.cat, levels=rev(c("normal","MAM","SAM"))))

#Weight-for-age (wfa) z-scores for children aged between zero and 120 months
#(Underweight)
df.wfa.i <- addWGSR(data = df2, sex = "sex", firstPart = "KG", secondPart = "age.days", index = "wfa") 
df.wfa <- df.wfa.i %>% filter(!is.na(wfaz)) %>% filter(wfaz <= 5) %>% filter(wfaz >= -5) %>% filter(!is.na(rdt.result))
df.wfa <- df.wfa %>% mutate(underweight.cat = case_when(
  wfaz >= -2 ~ "normal",
  wfaz <  -2 ~ "underweight")) %>%
  mutate(underweight.cat=factor(underweight.cat, levels=rev(c("normal","underweight"))))


#Height-for-age (hfa) z-scores for children aged between 24 and 228 months
#(Stunting)
df.hfa.i <- addWGSR(data = df2, sex = "sex", firstPart = "CM", secondPart = "age.days", index = "hfa") 
df.hfa <- df.hfa.i %>% filter(!is.na(hfaz)) %>% filter(hfaz <= 5) %>% filter(hfaz >= -5) %>% filter(!is.na(rdt.result))
df.hfa <- df.hfa %>% mutate(stunting.cat = case_when(
  hfaz >= -2 ~ "normal",
  hfaz <  -2 ~ "stunting")) %>%
  mutate(stunting.cat=factor(stunting.cat, levels=rev(c("normal","stunting"))))


#WASTING AND ACUTE MALNUTRITION
p.wfh <- df.wfh %>% group_by(time_point, wasting.cat) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=time_point, y=prop, fill = wasting.cat)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_viridis_d(option = "inferno", name = "Acute Malnutrition\nCategory", direction = -1, end = 0.8) +
  xlab("Time Point") + ylab("Proportion of children") +
  labs(title = "Acute Malnutrition: Weight for height data",
       subtitle = "(all sites comibined)") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

#STUNTING
p.hfa <- df.hfa %>% group_by(time_point, stunting.cat) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=time_point, y=prop, fill = stunting.cat)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_viridis_d(option = "inferno", name = "Stunting\nCategory", direction = -1, end = 0.8) +
  xlab("Time Point") + ylab("Proportion of children") +
  labs(title = "Stunting: Height for age data",
       subtitle = "(all sites comibined)") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

#UNDERWEIGHT
p.wfa <- df.wfa %>% group_by(time_point, underweight.cat) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=time_point, y=prop, fill = underweight.cat)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_viridis_d(option = "inferno", name = "Underweight\nCategory", direction = -1, end = 0.8) +
  xlab("Time Point") + ylab("Proportion of children") +
  labs(title = "Underweight: Weight for age data",
       subtitle = "(all sites comibined)") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

p.wfh + p.hfa + p.wfa



#############################################################################
# Weight for height (wfh) vs malaria infection status

p.wfh.mal1 <- df.wfh %>% group_by(rdt.result, site_code, wasting.cat) %>% summarize(n = n()) %>% mutate(prop = n/sum(n)) %>%
  ggplot(aes(x=factor(rdt.result), y=prop, fill = wasting.cat)) +
  geom_bar(position = "stack", stat="identity") +
  facet_wrap(vars(site_code), nrow = 1) +
  scale_fill_viridis_d(option = "inferno", name = "Acute Malnutrition\nCategory", direction = -1, end = 0.8) +
  xlab("Malaria infection status (1 = positive, 0 = negative)") + ylab("Proportion of children") +
  labs(title = "Acute Malnutrition vs Malaria Infection Status",
       subtitle = "(all sites comibined)") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

p.wfh.mal2 <- df.wfh %>% ungroup() %>%
  ggplot(aes(x=factor(rdt.result), y=wfhz)) +
  geom_jitter(alpha = 0.5, color = "grey40") +
  geom_boxplot(outlier.shape = NA, color = "black", alpha = 0) + 
  xlab("Malaria infection status\n(1 = positive, 0 = negative)") + 
  ylab("Weight for height z-score (wfhz)") +
  labs(title = "Comparing z-scores",
       subtitle = "(all sites comibined)") +
  theme_bw()

p.wfh.mal1 + p.wfh.mal2 + plot_layout(widths = c(5, 1))

