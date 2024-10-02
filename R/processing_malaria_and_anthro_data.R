library(tidyverse)
library(patchwork)
library(googlesheets4)
library(mgcv)
library(zscorer)




#########################################################################################################
# Read in data
#########################################################################################################
dfi <- readr::read_csv("/Users/blrice/Documents/GitHub/MANANJARY_MALARIA/data/rdt_hb_anthro_data.csv")

#########################################################################################################
# 1 # Calculate z scores
#########################################################################################################

df1 <- dfi %>% 
  #Recoding sex: 1 (male) or 2 (female)
  mutate(sex.coded = case_when(sex == "M" ~ 1, sex == "F" ~ 2)) %>%
  #Recoding age in days
    mutate(age.days = age.yrs.at.sample*365.25)

#Calculations: wfh, wfa, hfa

#Weight-for-height (wfh) z-scores for children with heights between 65 and 120 cm
#(Wasting)
#Severe Acute Malnutrition = SAM: wfhz score <-3
#Moderate Acute Malnutrition = MAM: wfhz score ≥ -3 and < -2 z-score
df1 <- addWGSR(data = df1, sex = "sex.coded", firstPart = "KG", secondPart = "CM", index = "wfh") 
df1 <- df1 %>% mutate(wasting.cat = case_when(
  wfhz >= -2 ~ "normal",
  wfhz <  -2 & wfhz >= -3 ~ "MAM",
  wfhz <  -3 ~ "SAM")) %>% 
  mutate(wasting.cat = factor(wasting.cat, levels = rev(c("normal","MAM","SAM"))))

#Weight-for-age (wfa) z-scores for children aged between zero and 120 months
#(Underweight)
df1 <- addWGSR(data = df1, sex = "sex.coded", firstPart = "KG", secondPart = "age.days", index = "wfa") 
df1 <- df1 %>% mutate(underweight.cat = case_when(
  wfaz >= -2 ~ "normal",
  wfaz <  -2 ~ "underweight")) %>%
  mutate(underweight.cat=factor(underweight.cat, levels=rev(c("normal","underweight"))))

#Height-for-age (hfa) z-scores for children aged between 24 and 228 months
#(Stunting)
df1 <- addWGSR(data = df1, sex = "sex.coded", firstPart = "CM", secondPart = "age.days", index = "hfa") 
df1 <- df1 %>% mutate(stunting.cat = case_when(
  hfaz >= -2 ~ "normal",
  hfaz <  -2 ~ "stunting")) %>%
  mutate(stunting.cat=factor(stunting.cat, levels=rev(c("normal","stunting"))))


#Test plotting
df1 %>% ggplot(aes(x = time_point, y = wfhz, color = site_code)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap(vars(site_code)) +
  theme_bw()

df1 %>% ggplot(aes(x = time_point, y = wfhz, color = wfhz)) +
  geom_jitter() +
  facet_wrap(vars(site_code)) +
  theme_bw()

#########################################################################################################
# 2 # Calculate MAM/SAM via PB/MUAC
#########################################################################################################

#Thresholds: For children 5y and younger
#   Moderate Acute Malnutrition (MAM):  PB/MUAC between 11.5 and 12.5
#   Severe Acute Malnutrition (SAM):    PB/MUAC < 11.5

df1 <- df1 %>% mutate(MUAC.cat = case_when(
  age.yrs.at.sample < 6 & PB >  12.5              ~ "normal",
  age.yrs.at.sample < 6 & PB >  11.5 & PB <= 12.5 ~ "MAM",
  age.yrs.at.sample < 6 & PB <= 11.5              ~ "SAM")) %>%
  mutate(MUAC.cat=factor(MUAC.cat, levels=rev(c("normal","MAM","SAM"))))


#########################################################################################################
# 3 # Calculate Anemia Categories
#########################################################################################################

#Applying anemia cut-offs

#Cut-offs used
# (ANE_A1) Newborns <  0.5 y                         = 'NA_Too_young'

# (ANE_B1) Children >= 0.5 y and <= 5 y : Hb  <  7.0 = '1_Severe'
# (ANE_B2) Children >= 0.5 y and <= 5 y : Hb  < 10.0 = '2_Moderate'
# (ANE_B3) Children >= 0.5 y and <= 5 y : Hb  < 11.0 = '3_Mild'
# (ANE_B4) Children >= 0.5 y and <= 5 y : Hb >= 11.0 = '0_Non_anemia'

# (ANE_C1) Children >    5 y and < 12 y : Hb  <  8.0 = '1_Severe'
# (ANE_C2) Children >    5 y and < 12 y : Hb  < 11.0 = '2_Moderate'
# (ANE_C3) Children >    5 y and < 12 y : Hb  < 11.5 = '3_Mild'
# (ANE_C4) Children >    5 y and < 12 y : Hb >= 11.5 = '0_Non_anemia'

# (ANE_D1) Children >=  12 y and < 15 y : Hb  <  8.0 = '1_Severe'
# (ANE_D2) Children >=  12 y and < 15 y : Hb  < 11.0 = '2_Moderate'
# (ANE_D3) Children >=  12 y and < 15 y : Hb  < 12.0 = '3_Mild'
# (ANE_D4) Children >=  12 y and < 15 y : Hb >= 12.0 = '0_Non_anemia'

# (ANE_E1) NP Females >= 15 y           : Hb  <  8.0 = '1_Severe'
# (ANE_E2) NP Females >= 15 y           : Hb  < 11.0 = '2_Moderate'
# (ANE_E3) NP Females >= 15 y           : Hb  < 12.0 = '3_Mild'
# (ANE_E4) NP Females >= 15 y           : Hb >= 12.0 = '0_Non_anemia'

# (ANE_F1) PR Females >= 15 y           : Hb  <  7.0 = '1_Severe'
# (ANE_F2) PR Females >= 15 y           : Hb  < 10.0 = '2_Moderate'
# (ANE_F3) PR Females >= 15 y           : Hb  < 11.0 = '3_Mild'
# (ANE_F4) PR Females >= 15 y           : Hb >= 11.0 = '0_Non_anemia'

# (ANE_G1) Males      >= 15 y           : Hb  <  8.0 = '1_Severe'
# (ANE_G2) Males      >= 15 y           : Hb  < 11.0 = '2_Moderate'
# (ANE_G3) Males      >= 15 y           : Hb  < 13.0 = '3_Mild'
# (ANE_G4) Males      >= 15 y           : Hb >= 13.0 = '0_Non_anemia'


# Using case_when() to filter age, sex, and anemia groups
df1 <- df1 %>%
  mutate(anemia.cat = case_when(
    
    age.yrs.at.sample  < 0.5 & !is.na(Hb)                             ~ 'ANE_A1',
    
    age.yrs.at.sample >= 0.5 & age.yrs.at.sample <=  5   & Hb <   7.0 ~ 'ANE_B1',
    age.yrs.at.sample >= 0.5 & age.yrs.at.sample <=  5   & Hb <  10.0 ~ 'ANE_B2',
    age.yrs.at.sample >= 0.5 & age.yrs.at.sample <=  5   & Hb <  11.0 ~ 'ANE_B3',
    age.yrs.at.sample >= 0.5 & age.yrs.at.sample <=  5   & Hb >= 11.0 ~ 'ANE_B4',
    
    age.yrs.at.sample >    5 & age.yrs.at.sample <  12   & Hb <   8.0 ~ 'ANE_C1',
    age.yrs.at.sample >    5 & age.yrs.at.sample <  12   & Hb <  11.0 ~ 'ANE_C2',
    age.yrs.at.sample >    5 & age.yrs.at.sample <  12   & Hb <  11.5 ~ 'ANE_C3',
    age.yrs.at.sample >    5 & age.yrs.at.sample <  12   & Hb >= 11.5 ~ 'ANE_C4',
    
    age.yrs.at.sample >=  12 & age.yrs.at.sample <  15   & Hb <   8.0 ~ 'ANE_D1',
    age.yrs.at.sample >=  12 & age.yrs.at.sample <  15   & Hb <  11.0 ~ 'ANE_D2',
    age.yrs.at.sample >=  12 & age.yrs.at.sample <  15   & Hb <  12.0 ~ 'ANE_D3',
    age.yrs.at.sample >=  12 & age.yrs.at.sample <  15   & Hb >= 12.0 ~ 'ANE_D4',
    
    age.yrs.at.sample >=  15 & sex == 'F' & Hb <   8.0 ~ 'ANE_E1',
    age.yrs.at.sample >=  15 & sex == 'F' & Hb <  11.0 ~ 'ANE_E2',
    age.yrs.at.sample >=  15 & sex == 'F' & Hb <  12.0 ~ 'ANE_E3',
    age.yrs.at.sample >=  15 & sex == 'F' & Hb >= 12.0 ~ 'ANE_E4',
    
    age.yrs.at.sample >=  15 & sex == 'M' & Hb <   8.0 ~ 'ANE_G1',
    age.yrs.at.sample >=  15 & sex == 'M' & Hb <  11.0 ~ 'ANE_G2',
    age.yrs.at.sample >=  15 & sex == 'M' & Hb <  13.0 ~ 'ANE_G3',
    age.yrs.at.sample >=  15 & sex == 'M' & Hb >= 13.0 ~ 'ANE_G4'))

df1 <- df1 %>%
  mutate(anemia.result = case_when(
    anemia.cat == 'ANE_A1' ~ 'NA_Too_young',
    
    anemia.cat == 'ANE_B1' ~ '1_Severe',
    anemia.cat == 'ANE_B2' ~ '2_Moderate',
    anemia.cat == 'ANE_B3' ~ '3_Mild',
    anemia.cat == 'ANE_B4' ~ '0_Non_anemia',
    
    anemia.cat == 'ANE_C1' ~ '1_Severe',
    anemia.cat == 'ANE_C2' ~ '2_Moderate',
    anemia.cat == 'ANE_C3' ~ '3_Mild',
    anemia.cat == 'ANE_C4' ~ '0_Non_anemia',
    
    anemia.cat == 'ANE_D1' ~ '1_Severe',
    anemia.cat == 'ANE_D2' ~ '2_Moderate',
    anemia.cat == 'ANE_D3' ~ '3_Mild',
    anemia.cat == 'ANE_D4' ~ '0_Non_anemia',
    
    anemia.cat == 'ANE_E1' ~ '1_Severe',
    anemia.cat == 'ANE_E2' ~ '2_Moderate',
    anemia.cat == 'ANE_E3' ~ '3_Mild',
    anemia.cat == 'ANE_E4' ~ '0_Non_anemia',
    
    anemia.cat == 'ANE_G1' ~ '1_Severe',
    anemia.cat == 'ANE_G2' ~ '2_Moderate',
    anemia.cat == 'ANE_G3' ~ '3_Mild',
    anemia.cat == 'ANE_G4' ~ '0_Non_anemia')) %>%
  mutate(anemia.result = factor(anemia.result, levels = c('1_Severe','2_Moderate','3_Mild','0_Non_anemia','NA_Too_young')))


#Test plotting
df1 %>% filter(!is.na(Hb)) %>% group_by(site_code, anemia.result) %>% 
  summarize(n = n()) %>%
  group_by(site_code) %>%
  mutate(prev =  n/sum(n) * 100) %>%
  ggplot(aes(x = site_code, y = prev, fill = anemia.result, group = site_code)) +
  geom_bar(stat = "identity") +
  theme_bw()


df1 %>% filter(!is.na(Hb)) %>% 
  ggplot(aes(x = site_code, fill = anemia.result)) +
  geom_bar(stat = ) +
  theme_bw()


##**** data cleaning: check for duplicate Hb measures


