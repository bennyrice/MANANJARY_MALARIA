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


###***Analysis/processing
###*Calculate anemia cats with flag for valid vs invalid
###*Calculate MAM/SAM via PB/MUAC

