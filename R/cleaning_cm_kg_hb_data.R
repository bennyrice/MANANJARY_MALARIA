library(tidyverse)
library(patchwork)
library(googlesheets4)
library(mgcv)


##############################################################################################################################
# Reading in data
##############################################################################################################################

#Read in data from google sheets

#URLs: Primary data entry
url.N1N5  <- "https://docs.google.com/spreadsheets/d/1XXlI372z-FzEPfOYfUwME7_1gWjguNd-aMePqv_zkOM/edit?usp=drive_link"
url.S1S2  <- "https://docs.google.com/spreadsheets/d/19pzwUSzTRoz0p9CAr9dke9cldtO7dtAmO0TuyJJHi1Q/edit?usp=drive_link"
url.S3    <- "https://docs.google.com/spreadsheets/d/1XYe4griKTW36LWzT8x-djgbhsQ06eeP_lVmRt3ZTaWg/edit?usp=drive_link"
url.S5    <- "https://docs.google.com/spreadsheets/d/1L7WdgHRFLtc-zsPfVOLaqN2XTDnGpDZLK-jD-9lUHQo/edit?usp=drive_link"
url.S6    <- "https://docs.google.com/spreadsheets/d/1ojJLlADexiSp-CPujyHb38xSykFcnXZ3nmqGg2cDb-I/edit?usp=drive_link"
#Read in
df.N1N5i  <- read_sheet(url.N1N5, col_types = "c")
df.S1S2i  <- read_sheet(url.S1S2, col_types = "c")
df.S3i    <- read_sheet(url.S3, col_types = "c")
df.S5i    <- read_sheet(url.S5, col_types = "c")
df.S6i    <- read_sheet(url.S6, col_types = "c")


#Cleaning up columns
cols_to_keep <- c("unique_ind_id",
                  "T01_MO","T01_DATY","T01_KG","T01_CM","T01_PB","T01_PC","T01_TEMP","T01_RDT_TIME","T01_RDT","T01_Hb",
                  "T02_MO","T02_DATY","T02_KG","T02_CM","T02_PB","T02_PC","T02_TEMP","T02_RDT_TIME","T02_RDT","T02_Hb",
                  "T03_MO","T03_DATY","T03_KG","T03_CM","T03_PB","T03_PC","T03_TEMP","T03_RDT_TIME","T03_RDT","T03_Hb",
                  "T04_MO","T04_DATY","T04_KG","T04_CM","T04_PB","T04_PC","T04_TEMP","T04_RDT_TIME","T04_RDT","T04_Hb",
                  "T05_MO","T05_DATY","T05_KG","T05_CM","T05_PB","T05_PC","T05_TEMP","T05_RDT_TIME","T05_RDT","T05_Hb",
                  "T06_MO","T06_DATY","T06_KG","T06_CM","T06_PB","T06_PC","T06_TEMP","T06_RDT_TIME","T06_RDT","T06_Hb",
                  "T07_MO","T07_DATY","T07_KG","T07_CM","T07_PB","T07_PC","T07_TEMP","T07_RDT_TIME","T07_RDT","T07_Hb",
                  "T08_MO","T08_DATY","T08_KG","T08_CM","T08_PB","T08_PC","T08_TEMP","T08_RDT_TIME","T08_RDT","T08_Hb",
                  "T09_MO","T09_DATY","T09_KG","T09_CM","T09_PB","T09_PC","T09_TEMP","T09_RDT_TIME","T09_RDT","T09_Hb",
                  "T10_MO","T10_DATY","T10_KG","T10_CM","T10_PB","T10_PC","T10_TEMP","T10_RDT_TIME","T10_RDT","T10_Hb",
                  "T11_MO","T11_DATY","T11_KG","T11_CM","T11_PB","T11_PC","T11_TEMP","T11_RDT_TIME","T11_RDT","T11_Hb")

col_renames_lookup <- c(T01_RDT.TIME = "T01_RDT_TIME",
                        T02_RDT.TIME = "T02_RDT_TIME", 
                        T03_RDT.TIME = "T03_RDT_TIME", 
                        T04_RDT.TIME = "T04_RDT_TIME", 
                        T05_RDT.TIME = "T05_RDT_TIME", 
                        T06_RDT.TIME = "T06_RDT_TIME", 
                        T07_RDT.TIME = "T07_RDT_TIME", 
                        T08_RDT.TIME = "T08_RDT_TIME", 
                        T09_RDT.TIME = "T09_RDT_TIME", 
                        T10_RDT.TIME = "T10_RDT_TIME", 
                        T11_RDT.TIME = "T11_RDT_TIME")

df.N1N5 <- df.N1N5i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S1S2 <- df.S1S2i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S3 <- df.S3i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S5 <- df.S5i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
df.S6 <- df.S6i %>%
  dplyr::select(any_of(cols_to_keep)) %>%
  rename(any_of(col_renames_lookup)) %>%
  pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))

##############################################################################################################################
# First data cleaning steps
##############################################################################################################################

#Binding together and cleaning up columns
df.primary <- rbind(df.N1N5, df.S1S2, df.S3, df.S5, df.S6) %>%
  mutate(variable = case_when(
    variable == "MO" ~ "month",
    variable == "DATY" ~ "day",
    variable == "RDT.TIME" ~ "RDT_TIME",
    variable == "RDT" ~ "rdt_result",
    .default = as.character(variable))) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
  dplyr::select(site_code, unique_ind_id:Hb) %>%
  arrange(site_code, time_point, unique_ind_id) %>%
  mutate(across(month:Hb, ~ ifelse(. == "NA", NA, .))) %>%
  mutate(across(c(KG:TEMP, Hb), as.numeric)) %>%
  mutate(across(c(day, RDT_TIME), as.integer))

#Checking for duplicate unique_ind_ids
df.primary %>% group_by(unique_ind_id, time_point) %>%
  summarize(n = n()) %>% filter(n > 1) 

df.primary <- df.primary %>%
  #Cleaning up data frame: site_code as a factor
  mutate(site_code = factor(site_code)) %>%
  #Dealing with invalid RDTs
  mutate(rdt_result.coded = case_when(
    rdt_result == "pan"   ~ 1,
    rdt_result == "pf"    ~ 1,
    rdt_result == "panpf" ~ 1,
    rdt_result == "neg"   ~ 0,
    rdt_result == ""      ~ NA_integer_,
    is.na(rdt_result)     ~ NA_integer_
    ))

##############################################################################################################################
# DATA CLEANING 1: Checking for errors in data entry
##############################################################################################################################

###*Note that QC duplicates done on N1, N2, N3 and approx. 25% of time points for other sites
###*Consider doing duplicates and triplicates?


#Read in quality control check data
#Data is piece-wise by site and time point in File 1; piecewise by site in File 2
#Have to read in each sheet 1 at a time

#URLs: Quality Control Check
#File 1: December 2023 Work
# url.qc.f1 <- "https://docs.google.com/spreadsheets/d/19bx8X9vjkCWTaTgOdBrMt0UIMbDoGZH9OrmTsqppm1I/edit?gid=0#gid=0"
#File 2: Spring 2024 Work
# url.qc.f2 <- "https://docs.google.com/spreadsheets/d/17Jyb6QF47_m5YdBO_JHlJG2lVVwuHMWXQRaigDv0zOo/edit?gid=0#gid=0"
#Read in all sheets/tabs within file using a function
#Sheet lists:
# v.f1 <- c("N1 T07", "N1 T10", "N2 T01", "N2 T07", "N2 T11", "N3 T03", "N3 T10", "N4 T06", "N4 T08", "N5 T10", "S1 T01", "S1 T02", "S1 T04", "S1 T07", "S1 T09", "S2 T04", "S2 T05", "S2 T08", "S3 T01", "S3 T04", "S3 T09", "S5 T05", "S5 T06", "S5 T10", "S6 T03", "S6 T06", "S6 T08")
# v.f2 <- c("N1", "N2", "N3")


f.qc.file_reader1 <- function(url, v.sheet.list){
  #Initialize an empty list to hold dfs
  l.out <- list(NA)
  #Loop through each sheet
  for(i in 1:length(v.sheet.list)){
    dfi <- read_sheet(url, sheet = v.sheet.list[i], col_types = "c") %>%
      #dropping dob_mo to making grabbing columns easier
      dplyr::select(-dob_mo) %>%
      #re-arranging columns and keeping columns using select(contains())
      #columns wanted: month, day, KG, CM, PB, PC, TEMP, RDT_TIME, RDT, Hb
      dplyr::select(contains("unique_ind_id"), 
                    contains("MO"), contains("DATY"), 
                    contains("KG"), contains("CM"), 
                    contains("PB"), contains("PC"), 
                    contains("TEMP"), contains("RDT_TIME"), contains("RDT"),
                    contains("Hb"))
    #Adding a time point column (and stripping the prefix from column names)
    dfi <- dfi %>% mutate(time_point = substr(names(dfi)[length(names(dfi))], 1, 3))
    names(dfi) <- c("unique_ind_id", 
                    "month", "day", 
                    "KG", "CM", 
                    "PB", "PC", 
                    "TEMP", "RDT_TIME", "rdt_result", 
                    "Hb", 
                    "time_point") 
    dfi <- dfi %>% dplyr::select("unique_ind_id", "time_point", "month", "day", "KG", "CM", "PB", "PC", "TEMP", "RDT_TIME", "rdt_result", "Hb")
     l.out[[i]] <- dfi
  }
  df.out <- bind_rows(l.out)
  return(df.out)
}

# df.f1 <- f.qc.file_reader1(url = url.qc.f1, v.sheet.list = v.f1)


#File 2: Spring 2024 Work

#Reading in
# df.f2.N1i <- read_sheet(url.qc.f2, sheet = "N1", col_types = "c")
# df.f2.N2i <- read_sheet(url.qc.f2, sheet = "N2", col_types = "c")
# df.f2.N3i <- read_sheet(url.qc.f2, sheet = "N3", col_types = "c")

#Cleaning up columns
# df.f2.N1 <- df.f2.N1i %>%
#   dplyr::select(any_of(cols_to_keep)) %>%
#   rename(any_of(col_renames_lookup)) %>%
#   pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
# df.f2.N2 <- df.f2.N2i %>%
#   dplyr::select(any_of(cols_to_keep)) %>%
#   rename(any_of(col_renames_lookup)) %>%
#   pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
# df.f2.N3 <- df.f2.N3i %>%
#   dplyr::select(any_of(cols_to_keep)) %>%
#   rename(any_of(col_renames_lookup)) %>%
#   pivot_longer(!unique_ind_id, names_sep = "_", names_to = c("time_point", "variable"))
# #Binding together and cleaning up columns
# df.f2 <- rbind(df.f2.N1, df.f2.N2, df.f2.N3) %>%
#   mutate(variable = case_when(
#     variable == "MO" ~ "month",
#     variable == "DATY" ~ "day",
#     variable == "RDT.TIME" ~ "RDT_TIME",
#     variable == "RDT" ~ "rdt_result",
#     .default = as.character(variable))) %>% 
#   pivot_wider(names_from = variable, values_from = value)
# 
# #Combining both sets of work and cleaning up
# df.qc <- rbind(df.f1, df.f2) %>%
#   mutate(site_code = substr(unique_ind_id, 1, 2)) %>%
#   dplyr::select(site_code, unique_ind_id:Hb) %>%
#   arrange(site_code, time_point, unique_ind_id) %>%
#   mutate(across(month:Hb, ~ ifelse(. == "NA", NA, .))) %>%
#   mutate(across(c(KG:TEMP, Hb), as.numeric)) %>%
#   mutate(across(c(day, RDT_TIME), as.integer))

#
#write_csv(df.qc, "/Users/blrice/Library/CloudStorage/Dropbox/Lab Projects/2020 Projects/CRS2020/1 DATA/DATA_ENTRY_WORK/20240916 Data Entry Resolutions/df.qc_20240924.csv")

########################################################################################################################
#Join primary data and quality control check data
# url.qc.read <- "https://docs.google.com/spreadsheets/d/18luemce_L9fVTUmWPcZGsfyAbpLXKm_xl5rTpQie2oQ/edit?gid=1889196570#gid=1889196570"
# 
# df.qc.read <- read_sheet(url.qc.read)
# 
# df.qc.read <- df.qc.read %>%
#   mutate(across(month:Hb, ~ ifelse(. == "NA", NA, .))) %>%
#   mutate(across(c(KG:TEMP, Hb), as.numeric)) %>%
#   mutate(across(c(day, RDT_TIME), as.integer))
# 
# df.joined <- full_join(df.primary, df.qc.read, by = join_by(unique_ind_id, time_point))
# 
# ########################################################################################################################
# #Compare for differences between primary data entry and quality control check data
# df.comp.month      <- df.joined %>% filter(month.x != month.y)
# df.comp.day        <- df.joined %>% filter(day.x != day.y)
# df.comp.KG         <- df.joined %>% filter(KG.x != KG.y)
# df.comp.CM         <- df.joined %>% filter(CM.x != CM.y)
# df.comp.PB         <- df.joined %>% filter(PB.x != PB.y)
# df.comp.PC         <- df.joined %>% filter(PC.x != PC.y)
# df.comp.TEMP       <- df.joined %>% filter(TEMP.x != TEMP.y)
# df.comp.RDT_TIME   <- df.joined %>% filter(RDT_TIME.x != RDT_TIME.y)
# df.comp.rdt_result <- df.joined %>% filter(rdt_result.x != rdt_result.y)
# df.comp.Hb         <- df.joined %>% filter(Hb.x != Hb.y)
# 
# #Discrepancies = 1 --> 0 [Resolved on 20240925]
# df.comp.month <- df.comp.month %>% dplyr::select(site_code.x:time_point, month.x, day.x, month.y, day.y)
# #Discrepancies = 66 --> 56 --> 43 --> 0 [Resolved on 20240925]
# df.comp.day <- df.comp.day %>% dplyr::select(site_code.x:time_point, month.x, day.x, month.y, day.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 120 --> 119 --> 3 --> 0 [Resolved on 20240926]
# df.comp.KG <- df.comp.KG %>% dplyr::select(site_code.x:time_point, KG.x, KG.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 94 --> 91 --> 90 --> 86 --> 5 --> 0 [Resolved on 20240926]
# df.comp.CM <- df.comp.CM %>% dplyr::select(site_code.x:time_point, CM.x, CM.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 16 --> 0 [Resolved on 20240925]
# df.comp.PB <- df.comp.PB %>% dplyr::select(site_code.x:time_point, PB.x, PB.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 15 --> 0 [Resolved on 20240925]
# df.comp.PC <- df.comp.PC %>% dplyr::select(site_code.x:time_point, PC.x, PC.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 70 --> 0 [Resolved on 20240926]
# df.comp.TEMP <- df.comp.TEMP %>% dplyr::select(site_code.x:time_point, TEMP.x, TEMP.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 128 --> 0 [Resolved on 20240925]
# df.comp.RDT_TIME <- df.comp.RDT_TIME %>% dplyr::select(site_code.x:time_point, RDT_TIME.x, RDT_TIME.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 36 --> 21 --> 12 --> 0 [All resolved on 20240925]
# df.comp.rdt_result <- df.comp.rdt_result %>% dplyr::select(site_code.x:day.x, rdt_result.x, rdt_result.y)
# #Discrepancies = 9 --> 0 [All resolved on 20240924]
# df.comp.Hb <- df.comp.Hb %>% dplyr::select(site_code.x:time_point, Hb.x, Hb.y)
# 
# ########################################################################################################################
# #Test for NA discrepancies : X is NA but Y has a value
# #Discrepancies = 4 --> 0 [All resolved on 20240926]
# df.comp.NAx.month      <- df.joined %>% filter(is.na(month.x) & !is.na(month.y))           %>% dplyr::select(site_code.x:time_point, month.x, month.y)           %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 3 --> 0 [All resolved on 20240926]
# df.comp.NAx.day        <- df.joined %>% filter(is.na(day.x) & !is.na(day.y))               %>% dplyr::select(site_code.x:time_point, day.x, day.y)               %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 6 --> 1 --> 0 [All resolved on 20240926]
# df.comp.NAx.KG         <- df.joined %>% filter(is.na(KG.x) & !is.na(KG.y))                 %>% dplyr::select(site_code.x:time_point, KG.x, KG.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 9 --> 7 --> 0 [All resolved on 20240926]
# df.comp.NAx.CM         <- df.joined %>% filter(is.na(CM.x) & !is.na(CM.y))                 %>% dplyr::select(site_code.x:time_point, CM.x, CM.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 12 --> 8 --> 0 [All resolved on 20240926]
# df.comp.NAx.PB         <- df.joined %>% filter(is.na(PB.x) & !is.na(PB.y))                 %>% dplyr::select(site_code.x:time_point, PB.x, PB.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 6 --> 3 --> 0 [All resolved on 20240926]
# df.comp.NAx.PC         <- df.joined %>% filter(is.na(PC.x) & !is.na(PC.y))                 %>% dplyr::select(site_code.x:time_point, PC.x, PC.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 2 --> 0 [All resolved on 20240926]
# df.comp.NAx.TEMP       <- df.joined %>% filter(is.na(TEMP.x) & !is.na(TEMP.y))             %>% dplyr::select(site_code.x:time_point, TEMP.x, TEMP.y)             %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 4 --> 3 --> 0 [All resolved on 20240926]
# df.comp.NAx.RDT_TIME   <- df.joined %>% filter(is.na(RDT_TIME.x) & !is.na(RDT_TIME.y))     %>% dplyr::select(site_code.x:time_point, RDT_TIME.x, RDT_TIME.y)     %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 3 --> 0 [All resolved on 20240926]
# df.comp.NAx.rdt_result <- df.joined %>% filter(is.na(rdt_result.x) & !is.na(rdt_result.y)) %>% dplyr::select(site_code.x:time_point, rdt_result.x, rdt_result.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 3 --> 0 [All resolved on 20240926]
# df.comp.NAx.Hb         <- df.joined %>% filter(is.na(Hb.x) & !is.na(Hb.y))                 %>% dplyr::select(site_code.x:time_point, Hb.x, Hb.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# 
# ########################################################################################################################
# #Test for NA discrepancies : X has a value but Y is NA
# #Discrepancies = 0
# df.comp.NAy.month      <- df.joined %>% filter(!is.na(month.x) & is.na(month.y) & !is.na(month.y))           %>% dplyr::select(site_code.x:time_point, month.x, month.y)           %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 0
# df.comp.NAy.day        <- df.joined %>% filter(!is.na(day.x) & is.na(day.y) & !is.na(month.y))               %>% dplyr::select(site_code.x:time_point, day.x, day.y)               %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 0
# df.comp.NAy.KG         <- df.joined %>% filter(!is.na(KG.x) & is.na(KG.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, KG.x, KG.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 8 --> 4 --> 0 [All resolved on 20240926]
# df.comp.NAy.CM         <- df.joined %>% filter(!is.na(CM.x) & is.na(CM.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, CM.x, CM.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 14 --> 0 [All resolved on 20240926]
# df.comp.NAy.PB         <- df.joined %>% filter(!is.na(PB.x) & is.na(PB.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, PB.x, PB.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 9 -- > 4 --> 0 [All resolved on 20240926]
# df.comp.NAy.PC         <- df.joined %>% filter(!is.na(PC.x) & is.na(PC.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, PC.x, PC.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 0
# df.comp.NAy.TEMP       <- df.joined %>% filter(!is.na(TEMP.x) & is.na(TEMP.y) & !is.na(month.y))             %>% dplyr::select(site_code.x:time_point, TEMP.x, TEMP.y)             %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 0
# df.comp.NAy.RDT_TIME   <- df.joined %>% filter(!is.na(RDT_TIME.x) & is.na(RDT_TIME.y) & !is.na(month.y))     %>% dplyr::select(site_code.x:time_point, RDT_TIME.x, RDT_TIME.y)     %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 4 --> 0 [All resolved on 20240926]
# df.comp.NAy.rdt_result <- df.joined %>% filter(!is.na(rdt_result.x) & is.na(rdt_result.y) & !is.na(month.y)) %>% dplyr::select(site_code.x:time_point, rdt_result.x, rdt_result.y) %>% arrange(time_point, site_code.x, unique_ind_id)
# #Discrepancies = 11 --> 0 [All resolved on 20240926]
# df.comp.NAy.Hb         <- df.joined %>% filter(!is.na(Hb.x) & is.na(Hb.y) & !is.na(month.y))                 %>% dplyr::select(site_code.x:time_point, Hb.x, Hb.y)                 %>% arrange(time_point, site_code.x, unique_ind_id)


##############################################################################################################################
# DATA CLEANING 2: Checking for data recording errors
##############################################################################################################################

#Adding an id code with time point specified for convenience
df.primary <- df.primary %>% mutate(full.code = paste0(unique_ind_id, ".", time_point)) %>%
  dplyr::select(site_code, unique_ind_id, full.code, time_point, month:rdt_result.coded)
##############################################################################################################################
# 2.1: RDT data
##############################################################################################################################

#Error cases:

#RDT result is not valid (pan, pf, panpf, neg)
table(df.primary$rdt_result)
#RDT result with no sample date recorded or sample date but no result
df.primary %>% filter((is.na(month) | is.na(day)) & !is.na(rdt_result.coded))
#List of known exceptions (verified samples where no RDT result expected)
v.known_RDT_no_samples <- c("N1.05.03.T01", "N2.01.05.T02", "N2.01.05.T03", 
                            "N4.45.02.T10", "S3.01.05.T10", "S6.24.02.T01", 
                            "S6.24.02.T03", "S6.24.02.T04", "S6.24.02.T05")
df.primary %>% filter(!is.na(month) & is.na(rdt_result) & KG > 9 & !(full.code %in% v.known_RDT_no_samples))

##############################################################################################################################
# 2.2: Anemia-hemoglobin data
##############################################################################################################################

#Error cases:
#Hemoglobin outside of plausible range
df.primary %>% filter(!is.na(Hb)) %>% filter(!is.na(KG)) %>%
  ggplot(aes(x = KG, y = Hb)) +
  geom_jitter() +
  theme_light()
#Hb result with no sample date recorded or sample date but no result
df.primary %>% filter((is.na(month) | is.na(day)) & !is.na(Hb))

##############################################################################################################################
# 2.3: Height data
##############################################################################################################################

#Error cases:
#Height outside of plausible range
hist(df.primary$CM)
#Height result with no sample date recorded or sample date but no result
df.primary %>% filter((is.na(month) | is.na(day)) & !is.na(CM))

##############################################################################################################################
# 2.4: Weight data
##############################################################################################################################

#Error cases:
#Weight outside of plausible range
hist(df.primary$KG)
#Weight result with no sample date recorded or sample date but no result
df.primary %>% filter((is.na(month) | is.na(day)) & !is.na(KG))
#No weight result but other data
#Hb
df.primary %>% filter(!is.na(Hb) & is.na(KG))
#List of known exceptions (verified samples where no weight result expected)
v.known_Weight_no_samples1 <- c("N1.04.01", "N1.08.04", "N2.01.05",
                                "N2.23.01", "N2.23.02", "N5.05.02",
                                "S2.02.01", "S2.09.03", "S5.04.11",
                                "S5.53.01")
df.primary %>% filter(!is.na(Hb) & is.na(KG) & !(unique_ind_id %in% v.known_Weight_no_samples1))
#RDT
df.primary %>% filter(!is.na(rdt_result) & is.na(KG) & !(unique_ind_id %in% v.known_Weight_no_samples1))
#List of known exceptions (verified samples where no weight result expected)
v.known_Weight_no_samples2 <- c("N2.03.01.T03", "N2.03.01.T05", "N2.20.08.T08", "N2.34.01.T08", "N2.51.03.T08", "N2.51.04.T08",
                                "N5.02.02.T02", "N5.02.02.T04", "N5.02.02.T05", "N5.02.02.T06", "N5.02.02.T07", "N5.02.02.T08",
                                "N5.06.02.T11", "S1.37.08.T07", "S2.51.02.T04", "S2.21.01.T09", "S3.56.01.T01", "S3.56.03.T01",
                                "S3.55.03.T08", "S3.04.06.T09", "S5.06.06.T02", "S5.28.03.T05", "S5.07.01.T07", "S5.61.02.T07",
                                "S5.07.01.T08", "S5.07.01.T09", "S6.16.03.T01", "S6.80.03.T01", "S6.04.01.T08", "S6.21.01.T08",
                                "S6.04.01.T09", "S6.21.01.T09", "S6.34.02.T09")
df.primary %>% filter(!is.na(rdt_result) & is.na(KG) & !(unique_ind_id %in% v.known_Weight_no_samples1) & !(full.code %in% v.known_Weight_no_samples2))

##############################################################################################################################
# 2.5: Height for weight data
##############################################################################################################################

#Error cases:
# HFW/WFH outside of plausible range
df.primary %>% filter(!is.na(CM)) %>% filter(!is.na(KG)) %>%
  ggplot(aes(x = KG, y = CM, color = site_code)) +
  geom_point(alpha = 0.7) +
  geom_smooth() +
  scale_color_viridis_d(option = "turbo") +
  theme_bw()
#Approx. 10 outliers to check***
df.primary.HFW <- df.primary %>% filter(!is.na(CM)) %>% filter(!is.na(KG)) %>% mutate(hfw = CM/KG, wfh = KG/CM)

#Using a gam to identify outliers
mod_gam1 = gam(wfh ~ s(KG), data = df.primary.HFW)
summary(mod_gam1)
#Some outliers apparent
hist(residuals.gam(mod_gam1))
#Adding the residual for each observation (using absolute value to look for largest outliers)
df.primary.HFW <- df.primary.HFW %>% mutate(abs.residuals.gam = abs(residuals.gam(mod_gam1)))

df.primary.HFW %>% 
  ggplot(aes(x = KG, y = wfh)) +
  geom_point(aes(color = abs.residuals.gam), alpha = 0.8) +
  geom_smooth() +
  scale_color_viridis_c(option = "inferno", end = 0.9) +
  theme_bw()
df.primary.HFW %>% filter(abs.residuals.gam > 0.05)
#Two individuals with high weight for age (seems validated by measurements)
v.known_Height_issues1 <- c("S1.14.02.T05", "S1.25.02.T03")
df.primary.HFW %>% filter(abs.residuals.gam > 0.05) %>% filter(!(full.code %in% v.known_Height_issues1))

#Given less variance at younger ages, check for outliers for KG < 40
df.primary.HFW %>%
  ggplot(aes(x = KG, y = abs.residuals.gam)) +
  geom_point(aes(color = abs.residuals.gam), alpha = 0.8) +
  scale_color_viridis_c(option = "inferno", end = 0.9) +
  theme_bw()
df.primary.HFW %>% filter(KG < 40) %>% filter(abs.residuals.gam > 0.035) %>% filter(!(full.code %in% v.known_Height_issues1))
v.known_Height_issues2 <- c("S6.94.09.T01", "S2.56.03.T03")
df.primary.HFW %>% filter(KG < 40) %>% filter(abs.residuals.gam > 0.030) %>% filter(!(full.code %in% v.known_Height_issues2))




##############################################################################################################################
# 2.6: Height and weight for age data
##############################################################################################################################

# Bring in age data


#Error cases:
#HFA/WFA outside of plausible range


##############################################################################################################################
# 2.7: Weight-Height Z scores
##############################################################################################################################

# Using z scores for relevant age groups



##############################################################################################################################
# 2.8: MUAC/PB data
##############################################################################################################################

#Error cases:
#MUAC/PB outside of plausible range
hist(df.primary$PB)

df.primary %>% filter(!is.na(PB)) %>% filter(!is.na(KG)) %>%
  ggplot(aes(x = KG, y = PB, color = site_code)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d(option = "turbo") +
  theme_bw()
#Approx. 5 outliers to address***

##############################################################################################################################
# 2.9: Cranial Circumference/PC data
##############################################################################################################################

#Error cases:
#PC outside of plausible range
hist(df.primary$PC)

df.primary %>% filter(!is.na(PC)) %>% filter(!is.na(KG)) %>%
  ggplot(aes(x = KG, y = PC, color = site_code)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d(option = "turbo") +
  theme_bw()
#Approx. 5 outliers to address***



##############################################################################################################################
# 2.10: Temperature data
##############################################################################################################################

#Error cases:
#TEMP outside of plausible range
hist(df.primary$TEMP)

df.primary %>% filter(!is.na(TEMP)) %>% 
  ggplot(aes(x = TEMP, y = 1, color = TEMP)) +
  geom_jitter(alpha = 0.7) +
  scale_color_viridis_c(option = "magma") +
  ylim(0, 2) +
  theme_bw()
#Approx. 2 outliers to address***


##############################################################################################################################
# 2.11: RDT Time data
##############################################################################################################################

#Error cases:
#TEMP outside of plausible range
hist(df.primary$RDT_TIME)

df.primary %>% filter(!is.na(RDT_TIME)) %>% 
  ggplot(aes(x = RDT_TIME, y = site_code, color = RDT_TIME)) +
  geom_jitter(alpha = 0.7) +
  scale_color_viridis_c(option = "mako") +
  facet_wrap(vars(time_point)) +
  theme_bw()
#Approx. 5 outliers to address***



###***After cleaning data for errors, tidy the data for downstream use
###*Drop z scores then recalculate in processing script
###*Add age and age cat and sex
###*recode RDT as 1 and 0

###***Analysis/processing
###*Calculate Z scores, with flag for valid vs invalid
###*Calculate anemia cats with flag for valid vs invalid
###*Calculate MAM/SAM via PB/MUAC





